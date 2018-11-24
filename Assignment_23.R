1. Perform the below given activities:
a. Take Apple Stock Prices from Yahoo Finance for last 90 days
b. Predict the Stock closing prices for next 15 days.
c. Submit your accuracy
d. After 15 days again collect the data and compare with your forecast

stringroptions(stringAsFactors=FALSE)
options(scipen=999)
options(java.parameters = "-Xmx6044m") ## memory set to 5GB

setwd("F:/AcadGild/workings")

lib=c("bigmemory", "readr", "Hmisc", "dplyr", "MASS", "ggplot2", "lattice", "caret", "rpart", 
      "randomForest", "rpart.plot","lattice", "rattle", "data.table","RColorBrewer", "reshape2",
      "InformationValue","stringr", "VIF", "Information", "Amelia", "gdata", "party","car", 
      "lubridate","zoo", "sqldf", "fuzzyjoin", "party", "mice")
sapply(lib, require, character.only=TRUE, quietly=TRUE)

df<-fread("F:/AcadGild/workings/AAPL-1Jul18-30Sep18.csv",sep=",",header=TRUE)

# exploratory analysis 
summary(df)
dim(df)
str(df)
describe(df)
names(df)
sapply(df, class)
head(df)
class(df)


format (df$Date, format="%d %m %Y")

new_date<- as.Date(df$Date)
new_date

format(new_date$Date, format="%B %d %Y")

data<-ts(df$Close, frequency = 90)

plot(data)

plot(data,main="Monthly Closing Prices")

log(data)

decompose(data) # default methode is additive

par(mfrow=c(1,2))
plot(decompose(data, type='multi'))

library(forecast)
seasonplot(data)

lag(data)

lag.plot(data)

# Calculation of Autocorrelation and Partial Autocorrelation
data

ac<-acf(data)

ac$acf

# data time series may not have stationarity

pac<-pacf(data)

pac$acf

# looking at the ACF and PACF graph we can conclude that the time series is not stationary

model <- lm(data~c(1:length(data)))

summary(model)

plot(resid(model),type='l')


# predictions 
pred<-predict(model, period=15)
pred
summary(pred)
describe(pred)

