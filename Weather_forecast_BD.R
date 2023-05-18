################Covid-19 forecasting in Bangladesh and Weather Impact###############
#                            Mohammad Nayeem Hasan                                 #
####################################################################################



library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

####time series


# Model building
setwd("E:\\ResearchProject\\Aminul\\Weather\\")
world <- read.csv("owid-covid-data.csv")
world_bd <- subset(world, world$location == "Bangladesh")
wdata <- read.csv("weather_data.csv")

world_bd <- subset(world_bd, world_bd$date >= "2021-01-25") #5/29/2021
world_bd <- subset(world_bd, world_bd$date <= "2022-01-25") #5/29/2021
world_bd$date
world_bd$CFR <- (world_bd$total_deaths/world_bd$total_cases)*100

bdwdata <- merge(world_bd, wdata, by=c("date"))


history <- data.frame(ds = seq(as.Date('2021-01-25'), as.Date('2022-01-25'), by = 'd'),
                      y = world_bd$CFR)
m3 <- prophet(history)
future <- make_future_dataframe(m3, periods = 30)
fcst3 <- predict(m3, future)
x <-plot(m3, fcst3, xlab="Months", ylab="Reported CFR (%)") + ggtitle("Prophet Model") + theme(
  plot.title = element_text(size=10))
plot(x)

last_fcst3 <- fcst3[366,]
rmse <- sqrt(mean((history$y - fcst3$yhat[c(1:366)])^2))
mae <- mean(abs((history$y - fcst3$yhat[c(1:366)])))
final <- cbind(last_fcst3, rmse, mae)
final


#R2
SSE <- sum((history$y - fcst3$yhat[c(1:366)])^2)
SST <- sum((history$y - mean(history$y))^2)
R_square <- 1 - SSE / SST
R_square

#ARIMA
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(ggplot2)

library(zoo)


myts <- ts(world_bd$CFR,start=c(2021), frequency = 365.25)

autoplot(myts)
auto.arima(myts)
Fit<-Arima(myts,order=c(0,2,1))
fcast <- forecast(Fit, h=30)
fcast$x
summary(Fit)

y <- autoplot(fcast, size = 1.5,geom = "point")  +
  autolayer(fcast$mean, series="Forecast", lwd = 0.6) +
  autolayer(fitted(Fit), series='Fitted', lwd = 0.6) + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("") + ylab("Reported CFR (%)") + ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="right") +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8),
         plot.title = element_text(size=9))

plot(y)
SSE <- sum((resid(fcast[1:366]))^2)
SST <- sum((world_bd$CFR[1:366] - mean(world_bd$CFR[1:366]))^2)
R_square <- 1 - SSE / SST
R_square

####SES########

library(tidyverse) 
library(fpp2) 



ses.goog <- ses(myts,
                h = 30) 
summary(ses.goog)

fcast <- forecast(ses.goog, h=30)
z <- autoplot(ses.goog, main=NULL)+
  autolayer(fcast$mean, series="Forecast") +
  autolayer(fitted(ses.goog), series='Fitted') + 
  autolayer(fcast$lower, series='lower') +
  autolayer(fcast$upper, series='upper') +
  xlab("") + ylab("Reported CFR (%)") + ggtitle("SES Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="right") +
  theme( legend.title = element_text(color = "Black", size = 8),
         legend.text = element_text(color = "Black", size = 8),
         plot.title = element_text(size=10))



z
y
gridExtra::grid.arrange(z,y,x)
dev.off()
accuracy(ses.goog)

#R2
SSE <- sum((resid(ses.goog[1:366]))^2)
SST <- sum((world_bd$CFR[1:366] - mean(world_bd$CFR[1:366]))^2)
R_square <- 1 - SSE / SST
R_square


#*****Beta Reg****#

library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(tidyverse)
library(betareg)
library(car)

## To disable scientific notation
options(scipen = 999)

bdwdata_pre <- subset(bdwdata, bdwdata$date >= "2021-01-25") #5/29/2021
bdwdata_pre <- subset(bdwdata, bdwdata$date <= "2021-07-27") #5/29/2021
bdwdata_pre$CFR <- bdwdata_pre$CFR/100

mydata <- data.frame(bdwdata_pre$ws,bdwdata_pre$temp,
                     bdwdata_pre$dew,bdwdata_pre$prec,bdwdata_pre$rh,bdwdata_pre$sp)

library(psych)
describe(mydata)


#Beta regression model 
model <- betareg(bdwdata_pre$CFR ~  bdwdata_pre$ws  + bdwdata_pre$temp + bdwdata_pre$dew + 
                   bdwdata_pre$prec + bdwdata_pre$rh + bdwdata_pre$sp, link="log", data = bdwdata_pre)

#Model summary
summary(model)

#IRR
round(exp(model$coefficients$mean),2)

#Confidence interval of IRR
round(exp(confint(model)),3)

bdwdata_pos <- subset(bdwdata, bdwdata$date >= "2021-07-28") #5/29/2021
bdwdata_pos <- subset(bdwdata, bdwdata$date <= "2022-01-25") #5/29/2021
bdwdata_pos$CFR <- bdwdata_pos$CFR/100

mydata <- data.frame(bdwdata_pos$ws,bdwdata_pos$temp,
                     bdwdata_pos$dew,bdwdata_pos$prec,bdwdata_pos$rh,bdwdata_pos$sp)

library(psych)
describe(mydata)

#Beta regression model 
model <- betareg(bdwdata_pos$CFR ~  bdwdata_pos$ws  + bdwdata_pos$temp + bdwdata_pos$dew + 
                   bdwdata_pos$prec + bdwdata_pos$rh + bdwdata_pos$sp, link="log", data = bdwdata_pos)

#Model summary
summary(model)

#IRR
round(exp(model$coefficients$mean),2)

#Confidence interval of IRR
round(exp(confint(model)),3)
