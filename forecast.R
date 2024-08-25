# Load necessary libraries
library(forecast)
library(tidyverse)
library(lubridate)

# Load the data from the CSV file, skipping the annotated lines
data <- read.csv("/Users/parsazn/Documents/TFG/Data-Collection-Services/EDMIDC-forecast-service/results.csv", comment.char = "#", skip = 3)

# Convert _time to POSIXct format to handle date and time
data$createdDate <- as.POSIXct(data$`X_time`, format="%Y-%m-%dT%H:%M:%OSZ")

# Ensure that the data is sorted by date and time
data <- data %>% arrange(createdDate)

# Create a time series object for the ARIMA model with 5-minute intervals
ts_data <- ts(data$`X_value`, frequency = 288)  # 288 intervals per day (5-minute intervals)

# Fit the ARIMA model
arima_model <- auto.arima(ts_data)

# Function to predict the price for a given date and time
predict_price <- function(datetime) {
  datetime <- as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S")
  
  # Calculate the number of 5-minute intervals from the last date-time in the dataset to the target date-time
  last_datetime <- max(data$createdDate)
  intervals_to_predict <- as.numeric(difftime(datetime, last_datetime, units = "mins")) / 5
  
  # Forecast the price
  forecast_result <- forecast(arima_model, h = intervals_to_predict)
  
  # Return the forecasted price for the target date-time
  return(forecast_result$mean[intervals_to_predict])
}

# Example: Predict the price for a specific date and time
target_datetime <- "2024-09-01 12:00:00"  # Replace with your target date and time
predicted_price <- predict_price(target_datetime)

cat("Predicted price for", target_datetime, "is:", predicted_price, "\n")
