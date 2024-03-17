install.packages("quadprog")
install.packages("quantmod")

install.packages("PerformanceAnalytics")
# Load necessary libraries
library(quantmod)
library(PerformanceAnalytics)

# Load the data
data <- read.csv("data.csv")

print("THe sample data is")
print(data)

# Calculate daily returns
symbols <- lapply(data$Symbol, function(x) {
  symbol_data <- tryCatch(
    getSymbols(x, src = "yahoo", from = "2010-01-01", to = "2023-09-30", auto.assign = FALSE),
    error = function(e) {
      message(paste("Failed to download data for symbol:", x))
      return(NULL)
    }
  )
  if (!is.null(symbol_data)) {
    return_series <- na.omit(ROC(Ad(symbol_data)))
    colnames(return_series) <- x
    return_series
  }
})

print("The symbols are")
print(symbols)

# Remove NULLs (failed downloads) from the list
symbols <- Filter(Negate(is.null), symbols)

# Continue with the remaining symbols
returns <- do.call(cbind, symbols)

print("The returns are")
print(returns)


# Install the package if not already installed
if (!"PerformanceAnalytics" %in% installed.packages()) {
  install.packages("PerformanceAnalytics")
}

# Load the package
library(PerformanceAnalytics)

# # Compute relative performance measures
market_returns <- returns[, "IVV"]
prin("The market returns are")
# Compute relative performance measures separately
JensenAlpha <- apply(returns, 2, function(x) CAPM.jensenAlpha(Ra = x, Rb = market_returns, Rf = 0))
MarketBeta <- apply(returns, 2, function(x) CAPM.beta(Ra = x, Rb = market_returns, Rf = 0))
TreynorRatio <- apply(returns, 2, function(x) (mean(x) - 0) / CAPM.beta(Ra = x, Rb = market_returns, Rf = 0))
TrackingError <- apply(returns, 2, function(x) {
  result <- tryCatch(TrackingError(Ra = x, Rb = market_returns), 
                     error = function(e) NA)
  return(result)
})
InformationRatio <- apply(returns, 2, function(x) {
  result <- tryCatch(InformationRatio(Ra = x, Rb = market_returns), 
                     error = function(e) NA)
  return(result)
})



# Combine the results into a data frame
relative_perf <- data.frame(JensenAlpha, MarketBeta, TreynorRatio, TrackingError, InformationRatio)

# Report relative performance measures in a summary table
summary_table <- data.frame(
  Mean = colMeans(relative_perf, na.rm = TRUE),
  Q1 = apply(relative_perf, 2, quantile, probs = 0.25, na.rm = TRUE),
  Median = apply(relative_perf, 2, median, na.rm = TRUE),
  Q3 = apply(relative_perf, 2, quantile, probs = 0.75, na.rm = TRUE)
)

# Print relative performance and summary table
print("The relative performance is")
print(relative_perf)
print("The summary table is")
print(summary_table)
# Identify worst and best-performing ETFs
rf <- 0.2
perf_summary <- data.frame(
  Mean = apply(returns, 2, mean),
  SD = apply(returns, 2, sd)
)
perf_summary$SR <- apply(returns, 2, function(x) {
  mean(x - rf) / sd(x - rf)
})

print(perf_summary)
# Plot asset means returns against their volatilities
plot(perf_summary$Mean, perf_summary$Volatility, xlab = "Mean Returns", ylab = "Volatility")
print(perf_summary$SR)
worst_etf <- which.min(perf_summary$SR)
print("worst_etf")
print(worst_etf)

best_etf <- which.max(perf_summary$SR)
print("best_etf")
print(best_etf)

print(paste("Best-performing ETF:", perf_summary$SR[best_etf]))
print(paste("Worst-performing ETF:", perf_summary$SR[worst_etf]))

# Analyze relationship between expense ratio and fund's performance
cor.test(data$Expense_Ratio, perf_summary$SR)

# Test CAPM relationship
finite_indices <- is.finite(perf_summary$Mean) & is.finite(relative_perf$MarketBeta)
plot(perf_summary$Mean[finite_indices], relative_perf$MarketBeta[finite_indices], xlab = "Mean Returns", ylab = "Beta")
# cor.test(perf_summary$Mean, relative_perf$MarketBeta)
