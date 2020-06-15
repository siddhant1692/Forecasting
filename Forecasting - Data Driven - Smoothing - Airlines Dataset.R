install.packages(c("forecast","fpp","smooth","tseries"))
library(forecast)
library(fpp)
library(smooth)
library(tseries)
library(readxl)

airlines<-read_xlsx("D:\\Data Science\\Excelr\\Assignments\\Assignment\\Forecasting\\Airlines+Data.xlsx") # Aviation.csv
View(airlines)
# Converting data into time series object
air<-ts(airlines$Passengers,frequency = 12,start=c(95))
View(air)
plot(air)

# dividing entire data into training and testing data 
train<-air[1:84]
test<-air[85:96] # Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data

# converting time series object
train<-ts(train,frequency = 12)
test<-ts(test,frequency = 12)
# Plotting time series data
plot(train) # Visualization shows that it has level, trend, seasonality => Additive seasonality

#### USING HoltWinters function ################
# Optimum values
# with alpha = 0.2 which is default value
# Assuming time series data has only level parameter
hw_a<-HoltWinters(train,alpha = 0.2,beta = F,gamma = F)

hwa_pred<-data.frame(predict(hw_a,n.ahead=12))
# By looking at the plot the forecasted values are not showing any characters of train data 
plot(forecast(hw_a,h=12))

hwa_mape<-MAPE(hwa_pred$fit,test)*100
# with alpha = 0.2, beta = 0.1
# Assuming time series data has level and trend parameter 
hw_ab<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = F)

hwab_pred<-data.frame(predict(hw_ab,n.ahead = 12))
# by looking at the plot the forecasted values are still missing some characters exhibited by train data
plot(forecast(hw_ab,h=12))
hwab_mape<-MAPE(hwab_pred$fit,test)*100
# with alpha = 0.2, beta = 0.1, gamma = 0.1 
# Assuming time series data has level,trend and seasonality 
hw_abg<-HoltWinters(train,alpha = 0.2,beta = 0.1,gamma = 0.1)

hwabg_pred<-data.frame(predict(hw_abg,n.ahead = 12))
# by looking at the plot the characters of forecasted values are closely following historical data
plot(forecast(hw_abg,h=12))
hwabg_mape<-MAPE(hwabg_pred$fit,test)*100
# With out optimum values 
hw_na<-HoltWinters(train,beta = F,gamma = F)

hwna_pred<-data.frame(predict(hw_na,n.ahead = 12))
hwna_pred
plot(forecast(hw_na,h=12))
hwna_mape<-MAPE(hwna_pred$fit,test)*100

hw_nab<-HoltWinters(train,gamma=F)

hwnab_pred<-data.frame(predict(hw_nab,n.ahead=12))
hwnab_pred
plot(forecast(hw_nab,h=12))
hwnab_mape<-MAPE(hwnab_pred$fit,test)*100

hw_nabg<-HoltWinters(train)

hwnabg_pred<-data.frame(predict(hw_nabg,n.ahead =12))
hwnabg_pred
plot(forecast(hw_nabg,h=12))
hwnabg_mape<-MAPE(hwnabg_pred$fit,test)*100
df_mape<-data.frame(c("hwa_mape","hwab_mape","hwabg_mape","hwna_mape","hwnab_mape","hwnabg_mape"),c(hwa_mape,hwab_mape,hwabg_mape,hwna_mape,hwnab_mape,hwnabg_mape))

colnames(df_mape)<-c("MAPE","VALUES")
View(df_mape)

# Based on the MAPE value who choose holts winter exponential tecnique which assumes the time series
# Data level, trend, seasonality characters with default values of alpha, beta and gamma

new_model <- HoltWinters(air)

plot(forecast(new_model,n.ahead=24))
# Forecasted values for the next 4 quarters
forecast_new <- data.frame(predict(new_model,n.ahead=12))
forecast_new

######## ARIMA Model ############# 

# Converting data into time series object
air<-ts(airlines$Passengers,frequency = 12,start=c(95))
View(air)
plot(air)

# dividing entire data into training and testing data 
train<-air[1:84]
test<-air[85:96] # Considering only 4 Quarters of data for testing because data itself is Quarterly
# seasonal data
# converting time series object
train<-ts(train,frequency = 12)
test<-ts(test,frequency = 12)
plot(train)
acf(train)
pacf(train)

# Auto.Arima model on the price agg data 
library(forecast)
model_AA <- auto.arima(train)
model_AA
pred_AA <- data.frame(forecast(model_AA))

acf(model_AA$residuals)
pacf(model_AA$residuals)
windows()
plot(forecast(model_AA,h=36),xaxt="n")




