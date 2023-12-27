# hymdata
#averageNO2
var1 <- All_Participants_pollution[,1]
result <- list()
for (i in 1:length(var1)) {
  table1 <- subset(All_Participants_pollution,eid==var1[[i]])[,c(1,2,8:10)]
  x <- ifelse(is.na(table1$`NO2- 2010- 24003-0.0`)==T,0,1)
  y <- ifelse(is.na(table1$`NO2-2005-24016-0.0`)==T,0,1)
  z <- ifelse(is.na(table1$`NO2-2006-24017-0.0`)==T,0,1)
  w <- ifelse(is.na(table1$`NO2-2007-24018-0.0`)==T,0,1)
  v <- x+y+z+w
  u <- (table1$`NO2- 2010- 24003-0.0`+table1$`NO2-2005-24016-0.0`+table1$`NO2-2006-24017-0.0`+table1$`NO2-2007-24018-0.0`)/v
  eid <- table1$eid
  res <- c(eid,u)
  names(res) <- c('eid','averageNO2')
  result <- rbind(result,res)
  print(i)
}
Result_NO2 <- as.data.frame(result)
Result_NO2$eid <- as.numeric(Result_NO2$eid)

Result_NO2$averageNO2 <- as.numeric(Result_NO2$averageNO2)
####averagePM10
var1 <- All_Participants_pollution[,1]
result <- list()
for (i in 1:length(var1)) {
  table2 <- subset(All_Participants_pollution,eid==var1[[i]])[,c(1,4,11)]
  x <- ifelse(is.na(table2$`PM10-2010-24005-0.0`)==T,0,1)
  y <- ifelse(is.na(table2$`PM10-2007-24019-0.0`)==T,0,1)
  v <- x+y
  u <- (table2$`PM10-2010-24005-0.0`+table2$`PM10-2007-24019-0.0`)/v
  eid <- table2$eid
  res <- c(eid,u)
  names(res) <- c('eid','averagePM10')
  result <- rbind(result,res)
  print(i)
}
Result_PM10 <- as.data.frame(result)
Result_PM10$eid <- as.numeric(Result_PM10$eid)


quantiles <- quantile(All_Participants_pollution$PM2.52010, c(1/4,2/4,3/4),na.rm = T)
All_Participants_pollution$averagePM2.5 <- NA
All_Participants_pollution$averagePM2.5[which(All_Participants_pollution$PM2.52010<quantiles[1])] <- 1
All_Participants_pollution$averagePM2.5[which(All_Participants_pollution$PM2.52010>=quantiles[1]&All_Participants_pollution$PM2.52010<quantiles[2])] <- 2
All_Participants_pollution$averagePM2.5[which(All_Participants_pollution$PM2.52010>=quantiles[2]&All_Participants_pollution$PM2.52010<quantiles[3])] <- 3
All_Participants_pollution$averagePM2.5[which(All_Participants_pollution$PM2.52010>=quantiles[3])] <- 4
quantiles <- quantile(All_Participants_pollution$NOX2010, c(1/4,2/4,3/4),na.rm = T)
All_Participants_pollution$averageNOX <- NA
All_Participants_pollution$averageNOX[which(All_Participants_pollution$NOX2010<quantiles[1])] <- 1
All_Participants_pollution$averageNOX[which(All_Participants_pollution$NOX2010>=quantiles[1]&All_Participants_pollution$NOX2010<quantiles[2])] <- 2
All_Participants_pollution$averageNOX[which(All_Participants_pollution$NOX2010>=quantiles[2]&All_Participants_pollution$NOX2010<quantiles[3])] <- 3
All_Participants_pollution$averageNOX[which(All_Participants_pollution$NOX2010>=quantiles[3])] <- 4
quantiles <- quantile(All_Participants_pollution$averageNO2.y, c(1/4,2/4,3/4),na.rm = T)
All_Participants_pollution$averageNO2_1.y <- NA
All_Participants_pollution$averageNO2_1.y[which(All_Participants_pollution$averageNO2.y<quantiles[1])] <- 1
All_Participants_pollution$averageNO2_1.y[which(All_Participants_pollution$averageNO2.y>=quantiles[1]&All_Participants_pollution$averageNO2.y<quantiles[2])] <- 2
All_Participants_pollution$averageNO2_1.y[which(All_Participants_pollution$averageNO2.y>=quantiles[2]&All_Participants_pollution$averageNO2.y<quantiles[3])] <- 3
All_Participants_pollution$averageNO2_1.y[which(All_Participants_pollution$averageNO2.y>=quantiles[3])] <- 4
quantiles <- quantile(All_Participants_pollution$averagePM10, c(1/4,2/4,3/4),na.rm = T)
All_Participants_pollution$averagePM10_1 <- NA
All_Participants_pollution$averagePM10_1[which(All_Participants_pollution$averagePM10<quantiles[1])] <- 1
All_Participants_pollution$averagePM10_1[which(All_Participants_pollution$averagePM10>=quantiles[1]&All_Participants_pollution$averagePM10<quantiles[2])] <- 2
All_Participants_pollution$averagePM10_1[which(All_Participants_pollution$averagePM10>=quantiles[2]&All_Participants_pollution$averagePM10<quantiles[3])] <- 3
All_Participants_pollution$averagePM10_1[which(All_Participants_pollution$averagePM10>=quantiles[3])] <- 4

quantiles <- quantile(All_Participants_pollution$X26260.0.0, c(1/5,4/5),na.rm = T)
All_Participants_pollution$PRS0.2 <- NA
All_Participants_pollution$PRS0.2[which(All_Participants_pollution$X26260.0.0<quantiles[1])] <- 1
All_Participants_pollution$PRS0.2[which(All_Participants_pollution$X26260.0.0>=quantiles[1]&All_Participants_pollution$X26260.0.0<quantiles[2])] <- 2
All_Participants_pollution$PRS0.2[which(All_Participants_pollution$X26260.0.0>=quantiles[2])] <- 3


mydata <- All_Participants_pollution
mydata$GROUP_PRS_NO2 <- NA
mydata$GROUP_PRS_NO2 <- paste0(mydata$PRS,'_',mydata$averageNO2_1.y)
mydata$GROUP_PRS_PM25 <- NA
mydata$GROUP_PRS_PM25 <- paste0(mydata$PRS,'_',mydata$averagePM2.5)
mydata$GROUP_PRS_PM10 <- NA
mydata$GROUP_PRS_PM10 <- paste0(mydata$PRS,'_',mydata$averagePM10_1)
mydata$GROUP_PRS_NOX <- NA
mydata$GROUP_PRS_NOX <- paste0(mydata$PRS,'_',mydata$averageNOX)


mydata <- mydata[!is.na(mydata$PRS),]
mydata <- mydata[!is.na(mydata$averageNO2_1.y),]
mydata <- mydata[!is.na(mydata$averagePM2.5),]
mydata <- mydata[!is.na(mydata$averagePM10_1),]
mydata <- mydata[!is.na(mydata$averageNOX),]
mydata <- subset(mydata,PD_date!='2037/7/7')
mydata <- subset(mydata,time)
mydata <- mydata[!is.na(mydata$'30000-0.0'),]

mydata$GROUP_PRS_NO2 <- factor(mydata$GROUP_PRS_NO2,levels = c('1_1','1_2','1_3','1_4','2_1','2_2','2_3','2_4','3_1','3_2','3_3','3_4'))
mydata$GROUP_PRS_PM25 <- factor(mydata$GROUP_PRS_PM25,levels = c('1_1','1_2','1_3','1_4','2_1','2_2','2_3','2_4','3_1','3_2','3_3','3_4'))
mydata$GROUP_PRS_PM10 <- factor(mydata$GROUP_PRS_PM10,levels = c('1_1','1_2','1_3','1_4','2_1','2_2','2_3','2_4','3_1','3_2','3_3','3_4'))
mydata$GROUP_PRS_NOX <- factor(mydata$GROUP_PRS_NOX,levels = c('1_1','1_2','1_3','1_4','2_1','2_2','2_3','2_4','3_1','3_2','3_3','3_4'))



#mydata <- merge(mydata,dataforHYM,by="eid")

model1 <- 'Age+Sex'
model2 <- 'Age+Sex+Townsand+college+BMI+Alcohal+Smoking+X6142+X738+Comorb+ethinic+genotyping'
model3 <- 'Age+Sex+Townsand+college+BMI+Alcohal+Smoking+X6142+X738+Comorb+ethinic+genotyping+PRS+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10'

####airpollution_PD
res.cox <- coxph(Surv(PD_years, PD_status) ~ averageNO2_1.y+model1, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ averageNO2_1.y+model2, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ averageNO2_1.y+model3, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ averageNOX+model1, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ averageNOX+model2, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ averageNOX+model3, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ averagePM2.5+model1, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ averagePM2.5+model2, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ averagePM2.5+model3, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ averagePM10_1+model1, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ averagePM10_1+model2, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ averagePM10_1+model3, data =  mydata)
summary(res.cox)

####airpollution_PD_PRS
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_NO2+model1, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_NO2+model2, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_NO2+model3, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_PM10+model1, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_PM10+model2, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_PM10+model3, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_PM25+model1, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_PM25+model2, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_PM25+model3, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_NOX+model1, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_NOX+model2, data =  mydata)
summary(res.cox)
res.cox <- coxph(Surv(PD_years, PD_status) ~ GROUP_PRS_NOX+model3, data =  mydata)
summary(res.cox)

