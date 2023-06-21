plot(modeling_data[,1],modeling_data[,2],xlim = c(0, 75),ylim = c(70, 225), main="Scatterplot of Fermenting Temperature vs Alcohol Percentage in Beer",xlab="Temperature (in degrees Fahrenheit)",ylab="Alcohol Content (in percent)")
mod1 = lm(Temperature ~ Alcohol_Percentage, data = modeling_data)
summ1 = summary(mod1)
summ1
plot(modeling_data$Temperature, summ1$residuals, ylab = "Residual Value", pch = 19, cex = 0.5)
identify(modeling_data$Temperature, summ1$residuals)
lev = hatvalues(mod1)
plot(modeling_data$Temperature, lev, ylab = "Leverage Value", pch = 19, cex = 0.5)
identify(modeling_data$Temperature, lev)

dif_betas = dfbeta(mod1)
#Change in Interecept Value 
plot(modeling_data$Temperature, dif_betas[,1], ylab = "Change in Intercept Value", pch = 19, cex = 0.5)
identify(modeling_data$Temperature, dif_betas[,1])
#Change in the Slope Value
plot(modeling_data$Temperature, dif_betas[,2], ylab = "Change in Slope Value", pch = 19, cex = 0.5)
identify(modeling_data$Temperature, dif_betas[,2])

modeling2 <- modeling_data[-186,]
mod2 <- lm(Alcohol_Percentage~Temperature, data=modeling2)
summ2 <- summary(mod2)
plot(modeling2[,1],modeling2[,2], main="Fermenting Temperature vs Alcohol Percentage in Beer",xlab="Temperature (in degrees Fahrenheit)",ylab="Alcohol Content (in percent)", pch = 19, cex = 0.5)
abline(mod2$coe[1], mod2$coe[2], col = 2)

actual<-validation_data$Alcohol_Percentage
predicted<-predict(mod2, newdata = validation_data)
plot(actual, predicted, xlab = 'Actual Alcohol Content', ylab = 'Predicted Alcohol Content', main = 'Actual vs. Predicted Alcohol Content', pch = 19, cex = 0.5)
abline(0,1, col = 2)

tx <- (0.02*(modeling2[,1]))^10
ty<- -(1/((modeling2[,2])^(1/2)))+1
plot(tx,ty, main="Fermenting Temperature vs Alcohol Percentage in Beer",xlab="Transformed Temperature Values",ylab=" Transformed Alcohol Content Values", pch = 19, cex = 0.5)

modeling3<-modeling2
modeling3[,1]<-tx
modeling3[,2]<-ty

mod3 <- lm(Alcohol_Percentage~Temperature, data=modeling3)
summ3 <- summary(mod3)
plot(modeling3[,1],modeling3[,2], main="Fermenting Temperature vs Alcohol Percentage in Beer",xlab="Transformed Temperature",ylab="Transformed Alcohol Content", pch = 19, cex = 0.5)
abline(mod3$coe[1], mod3$coe[2], col = 2)

plot(modeling2[,1],modeling2[,2], main="Fermenting Temperature vs Alcohol Percentage in Beer",xlab="Temperature (in degrees Fahrenheit)",ylab="Alcohol Content (in percent)", pch = 19, cex = 0.5)
abline(8.183426,-0.07771072, col = 2)

x<-NULL
y<-1/((0.3495686+0.0176676*( (0.02*(x))^10))^2)
actual<-validation_data$Alcohol_Percentage
validation_data2<-validation_data
validation_data2[,1]<- (0.02*(validation_data2[,1]))^10
validation_data2[,2]<- -(1/((validation_data2[,2])^(1/2)))+1
predictedty<-predict(mod3, newdata = validation_data2)
predicted<-1/(predictedty-1)^2
plot(actual, predicted, xlab = 'Actual Alcohol Content', ylab = 'Predicted Alcohol Content', main = 'Actual vs. Predicted Alcohol Content', pch = 19, cex = 0.5)
abline(0,1, col = 2)


PI <- predict(mod3, interval = "prediction", level = 0.95)
PI <- cbind(tx, PI)
PI = PI[order(PI[,1]),]
plot(modeling2[,1],modeling2[,2], main="Fermenting Temperature vs Alcohol Percentage in Beer",xlab="Temperature (in degrees Fahrenheit)",ylab="Alcohol Content (in percent)")
abline(8.183426,-0.07771072, col = 2)
#points((50*((PI[,1])^10)), (1/((PI[,3])-1)^2), type="l", lty=2, col = 3)
#points((50*((PI[,1])^10)), (1/((PI[,4])-1)^2), type="l", lty=2, col = 3)

library(ggplot2)
PI<-data.frame(PI)
ggplot(PI, aes(modeling2[,1], modeling2[,2]))+
  geom_point() +
  geom_line(aes(y=(1/((lwr)-1)^2)), color = "red", linetype = "dashed")+
  geom_line(aes(y=(1/((upr)-1)^2)), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)
