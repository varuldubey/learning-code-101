3+4
5+6/12
x<-15
y<-2*x+5
sqrt(2*(x+y))
6^2+2
x<-6^2+2
x^2
x
z<-c(1,2,3,4,5,6,7,8,9,10)
sum(z)
product(z)
??product
prod(z)
mean(z)
median(z)
p<-sqrt(sum(z))
z>5
z[2:5]
z[z>5]
z[z<=5]
z[z>mean(z)]
z[min(z)]
mean(ddtw$X90.110.mts)
table(ddtw$STATE)
median(ddtw$X90.110.mts)
ddtw$STATE
ddtw[2,]
mean(ddtw$X70.90.mts[ddtw$STATE=="BIHAR"])
bdtw <- ddtw$X70.90.mts[ddtw$STATE=="BIHAR"]
mean(bdtw)
BIHAR<-ddtw[ddtw$STATE=="BIHAR",]
library(SDSFoundations)
bike<-BikeData
bike[7,2]
bike$age[7]
bike$cyc_freq[1:10]
first10<-bike$cyc_freq[1:10]
View(first10)
table(first10)
bike[,6:9]
bike$speed[bike$cyc_freq=='Less than once a month']
mean(bike$distance[bike$student==1])
table(bike$student)
names(bike)
table(bike$cyc_freq[bike$student==1])
distance<-bike$distance[bike$student==1]
mean(bike$distance[bike$student==1])
student<-bike[bike$student==1,]
cyc_freq_st<-student$cyc_freq
table(cyc_freq_st)
mean(student$distance)
table(bike$cyc_freq)
table(bike$gender[bike$cyc_freq=="Daily"])
mean(bike$age[bike$cyc_freq=="Daily"])
dailyriders<-bike[bike$cyc_freq=="Daily",]
mean(dailyriders$age[dailyriders$gender=="M"])
mean(dailyriders$age[dailyriders$gender=="F"])
maleDR<-dailyriders[dailyriders$gender=="M",]
MT30YMDR<-maleDR[maleDR$age>=30,]
library(SDSFoundations)
animal<-AnimalData
rm(anmal)
head(animal)
plot(animal$Sex,main='Bar graph for animal gender',xlab='Animal Gender',ylab="frequency")
hist(animal$Age.Intake,main='Histogram for Age of Animal at intake',xlab='Age at intake',ylab="frequency")
A<-c(2,3,6,10,12,12,14,15,15,15,24,25)
hist(A)
rm(A)
maleweight<-animal$Weight[animal$Sex=="Male"]
femaleweight<-animal$Weight[animal$Sex=="Female"]
hist(animal$Weight,main='histogram for animal weights',xlab='weight at intake')
hist(maleweight,main='histogram for male animal weights',xlab='weight at intake',breaks = 15)
hist(femaleweight,main='histogram for female animal weights',xlab='weight at intake',breaks = 15)
max(femaleweight)
femaleweight[femaleweight>=80]
which(animal$Weight==80.5)
animal[207,]
which(animal$Weight==94)
animal[444,]
names(animal)
outcome<-animal$Outcome.Type[1:10]
table(outcome)
which(animal$Intake.Type=="Owner Surrender")
animal$Neutered.Status[7]
adopted <- animal$Days.Shelter[animal$Outcome.Type=="Adoption"]
hist(adopted,main='histogram for days spent in shelter by adopted animals',xlab = 'no. of days spent in shelter',breaks = 15)
fivenum(adopted)
mean(adopted)
#IQR = Q3-Q1
1.5*(38-8)
8-45
38+45
adopted[adopted>83]
adopted<-animal[animal$Outcome.Type=="Adoption",]
outliers<-adopted[adopted$Days.Shelter>83,]
boxplot(adopted$Days.Shelter)
(211-mean(adopted$Days.Shelter))/sd(adopted$Days.Shelter)
adults<-animal[animal$Age.Intake>=1,]
adultcatsweights<-adults$Weight[adults$Animal.Type=="Cat"]
adultdogsweights<-adults$Weight[adults$Animal.Type=="Dog"]
hist(adultcatsweights,main = 'histogram for weights of adult cats',xlab = 'weights of adult cats',ylab = 'no.of cats')
hist(adultdogsweights,main = 'histogram for weights of adult dogs',xlab = 'weights of adult dogs',ylab = 'no.of dogs')
mean(adultcatsweights)
sd(adultcatsweights)
fivenum(adultdogsweights)
#Z-Score  = (x - mean)/St.Dev.
(13-mean(adultcatsweights))/sd(adultcatsweights)
1-pnorm(2.3)
#percentile
lessthan13pounds<-adultdogsweights[adultdogsweights<13]
1-(53/226)
dogs<-animal[animal$Animal.Type=="Dog",]
table(dogs$Intake.Type)
81/291
ownersurrendered<-dogs[dogs$Intake.Type=="Owner Surrender",]
table(ownersurrendered$Outcome.Type)
which(ownersurrendered$Outcome.Type=="Return to Owner") 
daysspentatshelter<-ownersurrendered$Days.Shelter[ownersurrendered$Outcome.Type=="Return to Owner"]
mean(daysspentatshelter)
pnorm((8.79-6.7)/1.1)
pnorm((5.38-6.7)/1.1)
bull<-BullRiders
#Scatterplot
plot(bull$YearsPro,bull$BuckOuts14,main = 'Plot for no. of years Pro vs Buckouts in 2014',xlab = 'No. of years Pro',ylab = 'No. of buckouts')
abline(lm(bull$BuckOuts14~bull$YearsPro))
plot(bull$Events14,bull$BuckOuts14,main = 'Plot for no. of events contested vs Buckouts in 2014',xlab = 'No. of events contested',ylab = 'No. of buckouts')
abline(lm(bull$BuckOuts14~bull$Events14))
#Correlation coefficient
cor(bull$YearsPro,bull$BuckOuts14)
cor(bull$Events14,bull$BuckOuts14)
#correlation coefficient matrix
testvar<-c('YearsPro','Events14','BuckOuts14','Events13','BuckOuts13','Events12','BuckOuts12')
cor(bull[,testvar])
plot(bull$Events12,bull$BuckOuts12,main = 'Plot for no. of events contested vs Buckouts in 2012',xlab = 'No. of events contested',ylab = 'No. of buckouts')
abline(lm(bull$BuckOuts12~bull$Events12))
top15<-bull[bull$Rank15<=15,]
min(top15$BuckOuts14)
which(top15$BuckOuts14==1)
bull$Rides14[4]
bullsubset<-bull[bull$Events13>0,]
hist(bullsubset$Rides13)
fivenum(bullsubset$Rides13)
mean(bullsubset$Rides13)
plot(bull$Top10_13,bull$Rides13,main = 'Plot for times ranked in top 10 vs rides completed in 2013',xlab = 'No. of times in top 10',ylab = 'No. of rides completed')
abline(lm(bull$Rides13~bull$Top10_13))
cor(bull$Top10_13,bull$Rides13)
mean(bull$Rides13)
hist(bullsubset$Top10_13)
fivenum(bullsubset$Top10_13)
mean(bullsubset$Top10_13)
plot(bullsubset$Top10_13,bullsubset$Rides13,main = 'Plot for times ranked in top 10 vs rides completed in 2013',xlab = 'No. of times in top 10',ylab = 'No. of rides completed')
abline(lm(bullsubset$Rides13~bullsubset$Top10_13))
cor(bullsubset$Top10_13,bullsubset$Rides13)
which(bullsubset$Top10_13==2&bullsubset$Rides13==22)
bullsubset[4,]
boxplot(bullsubset$Rides13)
boxplot(bullsubset$Top10_13)
plot(bullsubset$Rides13,bullsubset$Top10_13,main = 'Plot for rides completed vs times ranked in top 10 in 2013',xlab = 'No. of rides completed',ylab = 'No. of times in top 10')
abline(lm(bullsubset$Top10_13~bullsubset$Rides13))
cor(bullsubset$Rides13,bullsubset$Top10_13)
bullsubset2<-bull[bull$Events12>0,]
hist(bullsubset2$Earnings12, main = 'histogram for Earnings in 2012', xlab = 'Earnings in 2012')
fivenum(bullsubset2$Earnings12)
mean(bullsubset2$Earnings12)
var<-c("RidePer12","CupPoints12","Earnings12")
cor(bullsubset2[,var])
plot(bullsubset2$RidePer12,bullsubset2$Earnings12, main = 'Scatterplot for Rides Perccentage vs Earnings in 2012',xlab = 'Percentage of rides completed', ylab = 'Earnings')
plot(bullsubset2$CupPoints12,bullsubset2$Earnings12, main = 'Scatterplot for Cup points vs Earnings in 2012',xlab = 'Cup points earned', ylab = 'Earnings')
which.max(bullsubset2$Earnings12)
bullsubset2[4,]
NoOutier<-bullsubset2[bullsubset2$Earnings12<1400000,]
boxplot(bullsubset2$Earnings12)
cor(NoOutier[,var])
plot(NoOutier$RidePer12,NoOutier$Earnings12)
plot(NoOutier$CupPoints12,NoOutier$Earnings12)
max(NoOutier$Earnings12)
boxplot(NoOutier$Earnings12)
bullsubset3<-bull[bull$Rides14>0,]
RidesPerEvent<-c(bullsubset3$Rides14/bullsubset3$Events14)
hist(RidesPerEvent,breaks = 10)
fivenum(RidesPerEvent)
boxplot(RidesPerEvent)
plot(RidesPerEvent,bullsubset3$Rank14, main = 'scatterplot for No. of Rides per event vs rank in 2014', xlab = 'average rides per event', ylab = 'rank')
abline(lm(bullsubset3$Rank14~RidesPerEvent))
cor(RidesPerEvent,bullsubset3$Rank14)
Time<-c(30,45,180,95,130,140,30,80,60,110,0,80)
Grade<-c(74,68,87,90,94,84,92,88,82,93,65,90)
cor(Time,Grade)
cor(Grade,Time)
plot(Time,Grade, main = 'scatterplot for time spent studying vs exam grades', xlab= 'time spent studying', ylab = 'exam grades')
abline(lm(Grade~Time))
Grade[1:7]
Time<-c(30,45,180,95,130,140,80,60,110,0,80)
Grade<-c(74,68,87,90,94,84,88,82,93,65,90)
cor(Time,Grade)
austin<-AustinCityLimits
tab<-table(austin$Gender,austin$Grammy)
prop.table(tab)
prop.table(tab,2)
rm(var)
newvar<-austin$Grammy[1:10]
table(newvar)
rm(newvar)
which(austin$Gender=="F" & austin$Age>60)
austin(10)
austin$Genre[10]
ao30<-austin[austin$Age>=30,]
newvar<-table(ao30$Gender,ao30$Genre)
prop.table(newvar,1)
barplot(newvar, legend.text = T,beside = T,main = 'Plot for genre wise distribution of gender',xlab = 'Genres',ylab = 'counts')
#Independence - P(A)=P(AlB)#
sum(ao30$Gender=="M")
sum(ao30$Gender=="F")
sum(0.07843137+0.51960784)
genre<-table(ao30$Genre)
prop.table(genre)
aclmale<-austin[austin$Gender=="M",]
genre<-table(aclmale$Genre)
grammy<-table(aclmale$Grammy)
cont<-table(aclmale$Grammy,aclmale$Genre)
prop.table(genre)
prop.table(grammy)
prop.table(cont,2)
barplot(prop.table(cont,2), legend.text = T, main = 'Plot for genre wise proportion of grammy winners',xlab = 'Genres',ylab = 'proportion')
popular<-table(austin$Facebook.100k)
agegrp<-table(austin$Age.Group)
cont<-table(austin$Facebook.100k,austin$Age.Group)
prop.table(agegrp)
prop.table(popular)
prop.table(cont,2)
barplot(prop.table(cont,2), legend.text = T, main = 'Plot for age group wise proportion of popular artists',xlab = 'Age Groups',ylab = 'proportion')
wr<-WorldRecords
View(wr)
mens800<-wr[wr$Event=="Mens 800m",]
linFit(mens800$Year,mens800$Record)
newvar<-mens800$Year-min(mens800$Year)
newvar
View(mens800)
linFit(newvar,mens800$Record)
table(wr$Event)
which(wr$Event=="Mens 100m" & wr$Athlete=="Usain Bolt")
wr$Year[15]
wr[15,]
which(wr$Event=="Womens Mile" & wr$Record<260)
wr$Athlete[146]
menshotput<-wr[wr$Event=="Mens Shotput",]
womenshotput<-wr[wr$Event=="Womens Shotput",]
newvar<-menshotput$Year-min(menshotput$Year)
newvar1<-womenshotput$Year-min(womenshotput$Year)
plot(newvar,menshotput$Record, main = 'scatterplot for mens shotput world record year vs distance', xlab= 'world record year', ylab = 'world recoed distance')
plot(newvar,menshotput$Record, main = 'scatterplot for mens shotput world record year vs distance', xlab= 'world record year', ylab = 'world record distance')
plot(newvar1,womenshotput$Record, main = 'scatterplot for womens shotput world record year vs distance', xlab= 'world record year', ylab = 'world record distance')
linFit(newvar,menshotput$Record)
linFit(newvar1,womenshotput$Record)
plot(newvar,menshotput$Record, main = 'scatterplot for mens shotput world record year vs distance', xlab= 'world record year', ylab = 'world record distance',pch=16)
linFit(menshotput$Year,menshotput$Record)
linFit(womenshotput$Year,womenshotput$Record)
mensmile<-wr[wr$Event==" Mens Mile",]
womensmile<-wr[wr$Event=="Womens Mile",]
mensmile<-wr[wr$Event=="Mens Mile",]
plot(mensmile$Year,mensmile$Record, main = 'scatterplot for mens mile world record', xlab= 'world record year', ylab = 'world recoed time (S)')
plot(womensmile$Year,womensmile$Record, main = 'scatterplot for womens mile world record', xlab= 'world record year', ylab = 'world recoed time (S)')
linFit(mensmile$Year,mensmile$Record)
linFit(womensmile$Year,womensmile$Record)
menspv70s<-wr[wr$Event=="Mens Polevault"& wr$Year>=1970,]
max(menspv70s$Record)
which(menspv70s$Record>6)
menspv70s$Year[26]
linFit(menspv70s$Year,menspv70s$Record)
newvar<-c(140,280,420,560)
newvar1<-c(0,2,4,6)
linFit(newvar1,newvar)
wb<-WorldBankData
us<-wb[wb$Country.Code=="USA",]
which(wb$IncomeGroup=="Low income")
wb$Country[54]
wb$rural.population[wb$Country=="Aruba" & wb$year==1970]
which(wb$Country=="Australia" & wb$mobile.users>0)
wb$year[611]
usfirstdecade<-us[us$year>=1990 & us$year<2000,]
time<-usfirstdecade$year-min(usfirstdecade$year)
expFit(time,usfirstdecade$internet.users)
logisticFit(time,usfirstdecade$internet.users)
expFitPred(time,usfirstdecade$internet.users,16)
logisticFitPred(time,usfirstdecade$internet.users,16)
#residuals#
205676834-127477387.967
205676834-3756413099.709
ussince1990<-us[us$year>=1990,]
time<-ussince1990$year-min(ussince1990$year)
expFit(time,ussince1990$internet.users)
logisticFit(time,ussince1990$internet.users)
e<-expFitPred(time,ussince1990$internet.users,16)
l<-logisticFitPred(time,ussince1990$internet.users,16)
205676834-e
205676834-l
max(ussince1990$internet.users)
wb$prop.int<-wb$internet.users/wb$population
denmarksince1990<-wb[wb$Country=="Denmark" & wb$year>=1990,]
denmarksince1990$yearsince1990<-denmarksince1990$year-min(denmarksince1990$year)
expFit(denmarksince1990$yearsince1990,denmarksince1990$prop.int)
logisticFit(denmarksince1990$yearsince1990,denmarksince1990$prop.int)
brazilsince1995<-wb[wb$Country=="Brazil" & wb$year>=1995,]
brazilsince1995$year<-brazilsince1995$year-min(brazilsince1995$year)
brazilsince1995$mobile.users<-brazilsince1995$mobile.users/1000000
expFit(brazilsince1995$year,brazilsince1995$mobile.users)
logisticFit(brazilsince1995$year,brazilsince1995$mobile.users)
brazilsince1995$mobile.users[brazilsince1995$year==5]
which(brazilsince1995$mobile.users>100)
brazilsince1995$year[13]
tripleFit(brazilsince1995$year,brazilsince1995$mobile.users)
logisticFitPred(brazilsince1995$year,brazilsince1995$mobile.users,30)
year<-c(1,3)
wolves<-c(25,45)
linFit(year,wolves)
expFit(year,wolves)
linFitPred(year,wolves,7)
expFitPred(year,wolves,7)
stsurvey<-StudentSurvey
hist(stsurvey$age)
mean(stsurvey$age)
sd(stsurvey$age)
sampleage<-sample(stsurvey$age, size = 20)
sampleage1<-sample(stsurvey$age, size = 30)
sampleage2<-sample(stsurvey$age, size = 40)
rm(sampleage)
rm(sampleage1)
mean(sampleage2)
sd(sampleage2)
samplemeans<-rep(NA,1000)
for (i in 1:1000) {
 sampleage<-sample(stsurvey$age, size = 60)
 samplemeans[i]<-mean(sampleage)
}
mean(samplemeans)
mean(stsurvey$age)
sd(samplemeans)
sd(stsurvey$age)/sqrt(60)
hist(samplemeans)
table(stsurvey$name_letters[stsurvey$ID<=10])
which(stsurvey$happy<40)
stsurvey$name_letters[11]
hist(stsurvey$name_letters)
mean(stsurvey$name_letters)
sd(stsurvey$name_letters)
samplemeans<-rep(NA,1000)
for (i in 1:1000) {
  samplename<-sample(stsurvey$name_letters, size = 5)
  samplemeans[i]<-mean(samplename)
}
hist(samplemeans, xlim = c(2,10))
mean(samplemeans)
sd(samplemeans)
sd(stsurvey$name_letters)/sqrt(5)
samplemeans<-rep(NA,1000)
for (i in 1:1000) {
  samplename<-sample(stsurvey$name_letters, size = 15)
  samplemeans[i]<-mean(samplename)
}
hist(samplemeans)
mean(samplemeans)
sd(samplemeans)
sd(stsurvey$name_letters)/sqrt(15)
for (i in 1:1000) {
  samplename<-sample(stsurvey$name_letters, size = 25)
  samplemeans[i]<-mean(samplename)
}
hist(samplemeans)
mean(samplemeans)
sd(samplemeans)
sd(stsurvey$name_letters)/sqrt(25)
hist(stsurvey$happy)
mean(stsurvey$happy)
sd(stsurvey$happy)
samplemeans<-rep(NA,1000)
for (i in 1:1000) {
  samplehappy<-sample(stsurvey$happy, size = 5)
  samplemeans[i]<-mean(samplehappy)
}
hist(samplemeans)
mean(samplemeans)
sd(samplemeans)
sd(stsurvey$happy)/sqrt(5)
for (i in 1:1000) {
  samplehappy<-sample(stsurvey$happy, size = 15)
  samplemeans[i]<-mean(samplehappy)
}
hist(samplemeans)
mean(samplemeans)
sd(samplemeans)
sd(stsurvey$happy)/sqrt(15)
for (i in 1:1000) {
  samplehappy<-sample(stsurvey$happy, size = 25)
  samplemeans[i]<-mean(samplehappy)
}
hist(samplemeans)
mean(samplemeans)
sd(samplemeans)
sd(stsurvey$happy)/sqrt(25)

hist(stsurvey$austin)
mean(stsurvey$austin)
sd(stsurvey$austin)
fivenum(stsurvey$austin)
samplemeans<-rep(NA,1000)
for (i in 1:1000) {
  sampleaustin<-sample(stsurvey$austin, size = 10)
  samplemeans[i]<-mean(sampleaustin)
}
hist(samplemeans)
mean(samplemeans)
sd(samplemeans)
sd(stsurvey$austin)/sqrt(10)
(3.2-3.08)/0.40
1- pnorm(0.3)
0.4/5
(3.2-3.08)/0.08
1-pnorm(1.5)
(2.9-3.08)/0.08
pnorm(1.5)-pnorm(-2.25)
11/sqrt(23)
(35.1-28)/2.29
1-pnorm(3.1)
qnorm(0.95)
1.5/sqrt(15)
qt(0.05,6)
weights<-bull$Weight[bull$Country=="USA"]
mean(weights)
sd(weights)
hist(weights)
t.test(weights,mu=190)
ridepercent<-bull$RidePer14[bull$Events14>=5]
mean(ridepercent)
sd(ridepercent)
hist(ridepercent)
t.test(ridepercent,mu=0.5)
bull$earnings_per<-bull$Earnings12/bull$Events12
hist(bull$earnings_per)
earnings_per<-log(bull$earnings_per[bull$Events12>0])
hist(earnings_per)
mean(earnings_per)
sd(earnings_per)
t.test(earnings_per,mu=10)
exp(8.572169)
exp(9.120605)
potatochips<-c(29.4,29.0,28.4,28.8,28.9,29.3,28.5,28.2)
mean(potatochips)
sd(potatochips)
(28.81-28.5)/(0.42/sqrt(8))
qt(0.025,7)
t.test(potatochips,mu=28.5)
hist(potatochips, xlim = c(25,33))
qt(0.975,17)
#Independent samples t-statistic#
(75.6-72.8)/sqrt((12.25/18)+(10.24/24))
husbands<-c(16,20,10,15,8,19,14,15)
wives<-c(15,18,13,10,12,16,11,12)
mean(husbands)
mean(wives)
sd(husbands)^2
sd(wives)^2
(mean(husbands)-mean(wives))/sqrt((16.55/8)+(7.41/8))
qt(0.025,7)
(78-66)/sqrt((157.75/10)+(144.96/15))
qt(0.95,9)
gumdiff<-c(-1,1,-2,-2)
sd(gumdiff)/sqrt(4)
mean(gumdiff)/0.707
qt(0.025,3)
post<-PostSurvey
#dependent two samples t-test#
diff<-post$exclusive-post$post_exclusive
hist(diff)
t.test(post$exclusive,post$post_exclusive,paired = T)
#independent two samples t-test#
msleep<-post$sleep_Tues[post$gender=="Male"]
fsleep<-post$sleep_Tues[post$gender=="Female"]
table(post$gender)
hist(msleep)
hist(fsleep)
t.test(msleep,fsleep)
which(post$gender=="Male")
post$classification[8]
table(post$live_campus[1:10])
underhappy<-post$happy[post$classification=="Freshman" |  post$classification=="Sophomore"]
upperhappy<-post$happy[post$classification=="Junior" | post$classification=="Senior"]
hist(underhappy)
hist(upperhappy)
t.test(underhappy,upperhappy)
diff<-post$post_happy-post$happy
hist(diff)
t.test(post$post_happy,post$happy,paired = T)
View(post)
diff<-post$hw_hours_college-post$hw_hours_HS
hist(diff)
t.test(post$hw_hours_college,post$hw_hours_HS,paired=T,alternative = "greater")
social<-post$sleep_Sat[post$greek=="yes"]
not_social<-post$sleep_Sat[post$greek=="no"]
hist(social)
hist(not_social)
t.test(social,not_social,alternative = 'less')
8.04-7.73
post$increase_hw<-post$hw_hours_college-post$hw_hours_HS
table(post$major)
biology<-post$increase_hw[post$major=='Biology']
nursing<-post$increase_hw[post$major=="Nursing"]
hist(biology)
hist(nursing)
t.test(nursing,biology)
qt(0.95,25)
sqrt((25/26)+(36/32))
6/1.44
qt(0.05,15)
left<-c(16.3,4.8,10.7,14,15.7,9.9,29.3,20.4,15.7,7.6,16.2,14.7,15,8.4,23.3,17.7)
right<-c(11.5,3.5,12.8,7.9,15.2,9.8,24,14.9,12.6,8.2,8.4,11,12.5,9.2,17.5,11.1)
diff<-left-right
hist(diff)
mean(diff)
sd(diff)
3.05/4
3.1/0.76
4.07*0.76
t.test(left,right,paired=T,alternative = 'greater')
1.753*0.76
3.1+1.33
3.1-1.33
#Chi square goodness of fit test#
grammytab<-table(austin$Grammy)
claim<-c(2/3,1/3)
chisq.test(grammytab,p=claim)
chisq.test(grammytab,p=claim)$expected
chisq.test(grammytab)
chisq.test(grammytab)$expected
grammytab
#Chi square test of independence#
grammyage<-table(austin$Age.Group,austin$Grammy)
grammyage
chisq.test(grammyage)$expected
chisq.test(grammyage,correct = F)
barplot(prop.table(grammyage,2))
prop.table(grammyage,2)
grammyage1<-table(austin$Grammy,austin$Age.Group)
barplot(prop.table(grammyage1,2))
prop.table(grammyage1,2)
artistgender<-table(austin$Gender)
artistgender
chisq.test(artistgender)
gendertop10<-table(austin$Gender,austin$BB.wk.top10)
gendertop10
chisq.test(gendertop10)$expected
chisq.test(gendertop10,correct = F)
prop.table(gendertop10,2)
barplot(prop.table(gendertop10,2))
gendertop101<-table(austin$BB.wk.top10,austin$Gender)
prop.table(gendertop101,2)
barplot(prop.table(gendertop101,2))
which(austin$Artist=="Allen Toussaint")
austin$Year[3]
austin$Age[3]
austin[3,]
genretab<-table(austin$Genre)
genretab
chisq.test(genretab)
barplot(genretab)
genre_twitter<-table(austin$Twitter.100k,austin$Genre)
genre_twitter
prop.table(genre_twitter,2)
twitter<-table(austin$Twitter.100k)
prop.table(twitter)
chisq.test(genre_twitter)
barplot(prop.table(genre_twitter,2),legend.text = T)
View(austin)
austin$Recent[austin$Year>=2012]<-1
austin$Recent[austin$Year<2012]<-0
recentgender<-table(austin$Recent,austin$Gender)
recentgender
chisq.test(recentgender)$expected
chisq.test(recentgender,correct = F)
prop.table(recentgender,1)
recentgender1<-table(austin$Gender,austin$Recent)
prop.table(recentgender1,2)
barplot(prop.table(recentgender1,2),legend.text = T)
offspring<-c(152,39,14)
claim<-c(0.75,0.15,0.10)
chisq.test(offspring,p=claim)
new<-New_Microsoft_Excel_Worksheet_3_
gender_hand<-table(new$Gender,new$`Dominant Hand`)
gender_hand
chisq.test(gender_hand)$expected
new<-New_Microsoft_Excel_Worksheet_3_
community_internet<-table(new$Community,new$Internet)
community_internet
chisq.test(community_internet,correct = F)
prop.table(community_internet,1)
community_internet1<-table(new$Internet,new$Community)
barplot(prop.table(community_internet1,2),legend.text = T)
#boxplot#
film<-FilmData
boxplot(film$Days)
boxplot(film$Days~film$Genre)
histogram(film$Days, xlab = "no. of days at box office")
#ANOVA tables#
aggregate(Days~Genre,film,mean)
aggregate(Days~Genre,film,var)
anovatab <- aov(film$Days~film$Genre) 
summary(anovatab)
TukeyHSD(anovatab)
table(film$Studio)
program<-data.frame(score)
score<-c(8,6,4,12,16,17,12,10,11,13,23,11,17,16,6,14,15,19,10,21,21,22,18,14,21,9,11)

aggregate(score~method,program,var)
anovatab<-aov(program$score~program$method)
summary(anovatab)
TukeyHSD(anovatab)
boxplot(program$score~program$method)
aggregate(Budget~Rating,film,mean)
aggregate(Budget~Rating,film,var)
boxplot(film$Budget~film$Rating, main = "boxplots for budget", xlab = "MRAA Rating", ylab = "movie budget")
anovatab<-aov(film$Budget~film$Rating)
summary(anovatab)
TukeyHSD(anovatab)
aggregate(IMDB~Rating,film,mean)
aggregate(IMDB~Rating,film,var)
boxplot(film$IMDB~film$Rating, main = "boxplots for IMDB rating", xlab = "MRAA Rating", ylab = "IMDB rating")
anovatab<-aov(film$IMDB~film$Rating)
summary(anovatab)
TukeyHSD(anovatab)
hist(film$Days)
aggregate(Days~Studio,film,mean)
aggregate(Days~Studio,film,var)
boxplot(film$Days~film$Studio, main = "Boxplots for No. of days at box office", xlab = "Studio", ylab = "No. of days")
anovatab<-aov(film$Days~film$Studio)
summary(anovatab)
TukeyHSD(anovatab)
table(film$Studio)
hist(film$Pct.Dom)
aggregate(Pct.Dom~Studio,film,mean)
aggregate(Pct.Dom~Studio,film,var)
boxplot(film$Pct.Dom~film$Studio, main = "Boxplots for domestic earnings as percentage of gross", xlab = "Studio", ylab = "Percent domestic earnings")
anovatab<-aov(film$Pct.Dom~film$Studio)
summary(anovatab)
TukeyHSD(anovatab)
film$cat_budget[film$Budget>=150]<-"high-budget"
table(film$cat_budget)
aggregate(Pct.Dom~cat_budget,film,mean)
aggregate(Pct.Dom~cat_budget,film,var)
boxplot(film$Pct.Dom~film$cat_budget, main = "Boxplots for domestic earnings as percentage of gross", xlab = "budget category", ylab = "Percent domestic earnings")
anovatab<-aov(film$Pct.Dom~film$cat_budget)
summary(anovatab)
TukeyHSD(anovatab)
#F distribution critical value#
qf(0.95,4,88)
#Chi-square distribution critical value#
qchisq(0.95,7)
#T distribution critical value#
qt(0.95,7)
#Z distribution critical value#
qnorm(0.95)
#Simple Linear Regression#
resiliance<-TempskiResilience
clerkship<-resiliance[resiliance$Group=="Clerkship",]
names(resiliance)
myvar<-c("BDI", "Resilience",  "State.Anxiety" , "Trait.anxiety")
corr.test(clerkship[,myvar])$p
linFit(clerkship$Resilience,clerkship$BDI)
linmod<-lm(BDI~Resilience, data = clerkship)
summary(linmod)
confint(linmod, level = 0.90)
lmBeta(linmod)
#Diagnostic plots for linear model#
#Residual vs fitted plot for Homoscedasticity#
plot(linmod,which=1)
clerkship[clerkship$IDR== "IDR1305",16:17]
#cook's distance plot for influential outliers#
cutoff<-4/linmod$df.residual
plot(linmod, which = 4, cook.levels = cutoff, id.n = 5)
#Remove influential outliers#
clerkship2<-clerkship[-c(33,87,109,245,339),]
#Multiple Linear Regression#
names(clerkship)
linmod<-lm(BDI~Resilience + State.Anxiety + Trait.anxiety, data = clerkship)
summary(linmod)
confint(linmod)
lmBeta(linmod)
pCorr(linmod)
corr.test(linmod)
#Residual vs fitted plot for Homoscedasticity#
plot(linmod,which=1)
#cook's distance plot for influential outliers#
cutoff<-4/linmod$df.residual
plot(linmod, which = 4, cook.levels = cutoff, id.n = 6)

clscience<-resilience[resilience$Group=="Clinical Sciences",]
cor(clscience$QoL,clscience$BDI, use = "pairwise.complete.obs")
linmod<-lm(QoL~BDI, data = clscience)
summary(linmod)
confint(linmod)
cor.test(clscience$BDI, clscience$QoL)
plot(linmod, which=1)
cutoff<-4/linmod$df.residual
plot(linmod, which=4, cook.levels = cutoff)
table(resilience$Group)
myvar<-c("MS.QoL","DREEM.A.SP","DREEM.S.SP","Resilience","BDI","Age")
corr.test(clscience[,myvar], use = "pairwise.complete.obs")
linmod<-lm(MS.QoL~DREEM.A.SP+DREEM.S.SP+Resilience+BDI+Age, data = clscience)
summary(linmod)
confint(linmod)
1/vif(linmod)
plot(linmod, which=1)
cutoff<-4/linmod$df.residual
plot(linmod,which = 4, cook.levels = cutoff, id.n = 4)
lmBeta(linmod)
round(pCorr(linmod),digits = 4)

basicsc<-resilience[resilience$Group=="Basic Sciences",]
myvar<-c("MS.QoL","WHOQOL.PH","WHOQOL.PSY","WHOQOL.SOC","WHOQOL.ENV")
corr.test(basicsc[,myvar], use = "pairwise.complete.obs")$r
linmod<-lm(MS.QoL~WHOQOL.PH+WHOQOL.PSY+WHOQOL.SOC+WHOQOL.ENV, data = basicsc)
summary(linmod)
lmBeta(linmod)
pCorr(linmod)

names(clscience)
linmod<-lm(BDI ~ Female + Age + State.Anxiety + Trait.anxiety, data = clscience)
summary(linmod)
lmBeta(linmod)
pCorr(linmod)

#List all available pre-loaded datasets in R packages#
data()

#Change class of a variable#
x<-c(1,1,2,2,2,3,4,4,5,5,5,5)
y<-as.character(x) #Convert numeric/factor class variable to character class#
y<-as.numeric(x) #Convert character/factor class variable to numeric class#
y<-factor(x) #Convert numeric/character class variable to factor class#

#List levels of a categorical variable stored as factors in alphabetical order#
levels(cyc_freq_st)

#Add numbers/integers in sequence of equal increments#
seq(1,10,2)
seq(1,11,length.out = 4)

#Sort data in vectors or data frames#
index<-order(murder$total)
murder$state[index]
murder[index,"state"]

#Calculate rank of each observation based on criteria#
murder$ranks<-rank(-murder$murder_rate) #use negative sign for highest to lowest rank#

#Row no./index of Maximum and minimum value of a numeric variable#
which.max(murder$total)
which.min(murder$total)

#Calculate how many 'NA' values in variable#
table(is.na(murder$population))
sum(is.na(murder$population)) #gives number of "TRUE" values in the logical vector#

#Indexing the missing values#
no.<-is.na(na_example)
index<-which(no.=="TRUE")

#logical functions to subset data#
which(murder$population<15000000) #returns indices for 'TRUE' logical values#
match(c("Boston","California"),murder$state) #returns indices for matching values in a vector#
c("Boston","California") %in% murder$state #returns logical argument for matching values in a vector#

#Data cleaning/wrangling using dplyr package#
murder<-mutate(murder, murder_rate=total/population*100000) #add/modify variables in the data frame#
my_df<-select(murder,state,region,population,murder_rate) #select specific columns to subset a data frame#
my_df<-filter(my_df,murder_rate>1 & population<mean(population)) #subset data frame based on conditions#
my_df<-murder %>% mutate(rate=total/population) %>% select(state,region, population, rate) %>% filter(rate>0.00001, population<mean(population)) #Pipe operator to feed result of one subsetting function to another#
my_df<-data.frame(state=c("Alabama","Arkansas","Alaska"),region=c("south","south","west"),population=c(4779736,2915918,710231),stringsAsFactors = F) #syntax for creating new data frame#

#Conditionals#
if (min(murder$murder_rate)<0.5) {
  murder$state[murder$murder_rate==min(murder$murder_rate)]
  } else {print("no states with murder rates less than 0.5 per 100k")} #If-Else statements for applying conditions on point estimates#
na_free<-ifelse(is.na(na_example),0,na_example) #If-Else for applying conditions on vectors# #replace NAs with 0#
any(is.na(na_free)) #check if at least one value in a logical vector is true# #check if values are missing in a variable#
all(ifelse(murder$murder_rate>0.5,TRUE,FALSE)) #check whether all values in a logical vector are true# #check if all values in a variable satisfy a condition#

#Creating functions#
reciprocal<-function(x){ifelse(x!=0,1/x,NA)} #add functions that perform repititive tasks#
y<-c(1,2,3,4,5,6,7,0,8)
reciprocal(y)

#For loop#
random<-vector(mode="numeric",1000)
for (i in 1:1000) {sample_random<-sample(1:1000,50,replace = F)
random[i]<-mean(sample_random)
}                              #useful for carrying out simulations of simple random sampling#                               

#Other useful functions# #TO BE LEARNED SEPARATELY#
apply(array, margin, ...)
sapply(list, function)
tapply(vector, index, function)
mapply(function, ...)
split()
cut()
quantile(x,c(0.1,0.5)) #calculate value of variable below which lies a given proportion of data#
Reduce()
identical(x,y) #determine whether two objects are identical, cannot directly test functions stored as objects#
unique(student$age) #identify the unique values in a variable#
nchar(murder$state) #number of characters in a character vector, counts space as character#
pnorm(72,mean = 69,sd = 2.5)-pnorm(69,mean = 69,sd = 2.5) #syntax for calculating approximate proportion between values without calculating respective z scores#
mad(x) #calculate the median absolute deviation as a substitute to standard deviation#
mean(na_example,na.rm = T) #"na.rm" ignores 'NA' values while computing summary statistic#
reorder(region,per_capita_gdp_pd,FUN = median) #Reorder categorical variable based on numerical variable#

#Check the Normal Distribution approximation#
hist(heights$height[heights$sex=="Male"]) #histogram for determining approximation to a normal distribution#
p<-seq(0.05,0.95,0.05)
z<-scale(x) #convert the values in x to z-scores#
plot(quantile(x,p),qnorm(p,mean = mean(x),sd = sd(x))) #q-q plot for determining approximation to a normal distribution#
plot(qnorm(p),quantile(z,p)) #q-q plot on z-scale for determining approximation to a normal distribution#
abline(0,1) #Fitting a theoretical straight line through the graph#

#Creating visual representations using ggplot2#
#Scatterplot#
p<-ggplot(data = murders,mapping = aes(x = population/10^6,y = total, label = abb)) #defines a ggplot object associated with data table and Globally mapped with x and y variable#
p+geom_point(size = 2, colour = "blue") #Add layer of geometry (type of plot) and aesthetics such as size and colour of points#
q<-p+geom_point()+geom_text_repel(size = 4) #Add layer of text/label to the data points and aesthetics such as size and position of text#
r<-q+scale_x_continuous(trans = "log10")+scale_y_continuous(trans = "log10") #transform scales of both axes#
s<-r+xlab("Population in Millions(log scale)")+ylab("Total Murders(log scale)")+ggtitle("Gun Murders in US in 2010") #change axes labels and add title#
t<-s+geom_point(aes(col=region),size = 4) #map different colour codes to states belonging to different regions#
u<-t+geom_abline(intercept = log10(sum(murders$total)/sum(murders$population)*10^6), lty = 2, colour = "red") #Add a line of average US murder rate and aesthetics such as type and colour of line#
v<-u+scale_color_discrete(name = "Region") #change title of legend#
v+theme_economist() #add background theme to the graph#

#Histogram#
p<-ggplot(data = heights[heights$sex=="Male",], aes(x=height)) #defines a ggplot object associated with male heights data and global mapping of x variable#
q<-p+geom_histogram(binwidth = 1, fill = "green", col = "black") #add layer of geometry (histogram plot) and aesthetics such as bandwidth and colour of bars#
q+xlab("Male Heights")+ylab("Frequency")+ggtitle("Histogram for Male Heights") #change axes labels and title#

#Smooth Desnity Plot#
p<-ggplot(data = heights[heights$sex=="Male",], aes(x=height)) #defines a ggplot object associated with male heights data and global mapping of x variable#
q<-p+geom_density(fill = "skyblue") #add layer of geometry (smooth density plot) and aesthetics such as colour for area inside the plot#
q+xlab("Male Heights")+ylab("Density")+ggtitle("Smooth Density plot for Male Heights") #change axes labels and title#

p<-ggplot(data = heights, aes(x=height)) #defines a ggplot object associated with heights data and global mapping of x variable#
q<-p+geom_density(aes(fill = sex),alpha = 0.2) #add layer of geometry (smooth density plot) and map different colours to male and female heights to differentiate their distributions within same plot and adjusting aesthetics such as alpha level#
q+xlab("Heights")+ylab("Density")+ggtitle("Smooth Density plots for Male and Female Heights compared") #change axes labels and plot title#

#Q-Q plot#
p<-ggplot(data = heights[heights$sex == "Male",], aes(sample = height)) #defines a ggplot object associated with heights data and global mapping of sample of heights#
q<-p+geom_qq(dparams = c(mean(x),sd(x))) #add layer of geometry (q-q plot) and supplied data parameters (mean and sd) for caluclating theoretical quantiles#
r<-q+ylab("observed")+ggtitle("q-q plot for Male Heights") #change axes labels and title#
r+geom_abline() #Fitting a theoretical straight line through the graph#

p<-ggplot(data = heights[heights$sex == "Male",], aes(sample = scale(height))) #defines a ggplot object associated with heights data and global mapping of sample of heights converted to z-scores#
q<-p+geom_qq() #add layer of geometry (q-q plot)# #mean and sd not required as data is converted to z-scores#
r<-q+ylab("observed")+ggtitle("q-q plot for Male Heights") #change axes labels and title#
r+geom_abline() #Fitting a theoretical straight line through the graph#

#Comparing two or more plots#
p1<-ggplot(data = heights[heights$sex=="Male",], aes(x=height))+geom_density(fill = "skyblue")+xlab("Male Heights")+ylab("Density")+ggtitle("Smooth Density plot for Male Heights") #Smooth density plot for male heights#
p2<-ggplot(data = heights[heights$sex=="Female",], aes(x=height))+geom_density(fill = "skyblue")+xlab("Female Heights")+ylab("Density")+ggtitle("Smooth Density plot for Female Heights") #Smooth density plot for female heights#
grid.arrange(p1,p2, ncol = 2) #Arrange two plots side-by-side for comparison#

#Exploratory data summary using dplyr#
summ<-summarise(heights,average = mean(height),median = median(height),standard_deviation = sd(height),median_absolute_deviation = mad(height)) #calculate and store point summary estimates in a data frame#
new<-summ %>% .$average #using dot placeholder to return vector output instead of data frame, useful when the vector output is to be used as input for a function#
summ<-group_by(murder,region) %>% summarise(average_rate = mean(murder_rate),median_rate = median(murder_rate)) #summarize by one or more groups or factors#
new<-arrange(murder,region,desc(murder_rate)) #Sort entire data frame by one or more variables#

#Data visualisation techniques#
#Faceting#
filter(gap_minder,year %in% c(1962,2012), !is.na(fertility),!is.na(life_expectancy)) %>% ggplot(aes(x = fertility,y = life_expectancy, col = continent)) + geom_point(size = 3) + facet_wrap(continent~year) #faceting same graph by 2 or more stratas/variables#

#Time-series plots#
filter(gap_minder,country %in% c("Poland","South Korea"),!is.na(life_expectancy)) %>% ggplot(aes(x = year,y = life_expectancy, col = country)) + geom_line() #compare two or more rows on a measurable parameter over a period of time#
filter(gap_minder,continent=="Asia",!is.na(life_expectancy)) %>% group_by(region,year) %>% summarise(life_expectancy = mean(life_expectancy)) %>% ggplot(aes(x = year,y = life_expectancy, col = region)) + geom_line() #compare two or more groups on a measurable parameter over a period of time#

#Transformation#
filter(gap_minder,year==1962,is.na(per_capita_gdp)==F) %>% mutate(per_capita_gdp_pd=per_capita_gdp/365) %>% ggplot(aes(x = log2(per_capita_gdp_pd))) + geom_histogram(binwidth = 1,col = "black") #transforming data that does not approximate normal distribution to appropriate log scale for comparison#
filter(gap_minder,year==1962,!is.na(per_capita_gdp)) %>% mutate(per_capita_gdp_pd=per_capita_gdp/365) %>% ggplot(aes(x = per_capita_gdp_pd)) + geom_histogram(binwidth = 1,col = "black") + scale_x_continuous(trans = "log2") #transforming scale of x axis to appropriate log scale for comparing distribution that does not approximate normal distribution#

#Sratifying and Boxplots#
filter(gap_minder,year==1962,!is.na(per_capita_gdp)) %>% mutate(per_capita_gdp_pd = per_capita_gdp/365, region = reorder(region,per_capita_gdp_pd,FUN = median)) %>% ggplot(aes(x = region, y = per_capita_gdp_pd)) + geom_boxplot(aes(fill=continent)) + geom_point() + scale_y_continuous(trans = "log2") + theme(axis.text.x = element_text(angle = 90,hjust = 1)) + xlab("") + ylab("Per Capita GDP Per Day") #Boxplots stratified by region variable, ordered based on median per capita gdp, with data points indicating countries and aesthetics such as names of regions rotated vertically#

#Visually comparing distributions across years using Histograms#
west<-c("Australia and New Zealand","Northern Europe","Northern America","Western Europe","Southern Europe") #define western regions for grouping the world's countries#
past_country<-filter(gap_minder,year==1970,!is.na(per_capita_gdp)) %>% .$country %>% as.character() #creating a list of countries that had gdp figures in 1970#
present_country<-filter(gap_minder,year==2010,!is.na(per_capita_gdp)) %>% .$country %>% as.character() #creating a list of countries that had gdp figures in 2010#
countries<-intersect(past_country,present_country) #creating list of countries that had gdp figures for both 1970 and 2010#
filter(gap_minder,year %in% c(1970,2010),country %in% countries, !is.na(per_capita_gdp)) %>% mutate(per_capita_gdp_pd = per_capita_gdp/365, group = ifelse(region %in% west,"western","Rest of the world")) %>% ggplot(aes(x = per_capita_gdp_pd)) + geom_histogram(binwidth = 1, col = "black") + scale_x_continuous(trans = "log2") + facet_wrap(year~group) #Histograms for per capita gdp, which only include countries that have data for both years, faceted by two variables viz. group(RestvsWest) and year#

#Visually comparing distributions across years using Boxplots#
filter(gap_minder,year %in% c(1970,2010), !is.na(per_capita_gdp)) %>% mutate(per_capita_gdp_pd = per_capita_gdp/365, region = reorder(region,per_capita_gdp_pd,FUN = median)) %>% ggplot(aes(x = region, y = log2(per_capita_gdp_pd), fill = factor(year))) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90,hjust = 1)) + xlab("") #Boxplots showing region wise per capita gdp for two years, put side by side, and colour mapped with years converted into factors#

#Visually comparing distributions across years using Density plots#
filter(gap_minder,year %in% c(1970,2010),country %in% countries, !is.na(per_capita_gdp)) %>% mutate(per_capita_gdp_pd = per_capita_gdp/365,group = ifelse(region %in% west,"western","Rest of the world")) %>% ggplot(aes(x = per_capita_gdp_pd, y = ..count.., fill = group)) + geom_density(alpha = 0.2, bw = 0.75) + scale_x_continuous(trans = "log2") + facet_wrap(.~year) #Density plots colour mapped by group(WestvsRest), faceted by year, showing counts instead of densities on y-axis, with increased smoothening#

gap_minder<-gap_minder %>% mutate(group = case_when(.$region %in% west ~ "Western",.$region %in% c("Eastern Asia","South-Eastern Asia") ~ "East Asia",.$region %in% c("Caribbean","Central America","South America") ~ "Latin America",.$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa", TRUE ~ "Others")) #Creating new set of regions using 'case_when' function#
gap_minder$group<-factor(gap_minder$group,levels=c("Others","Latin America","East Asia","Sub-Saharan Africa","Western")) #Redefining regions created above as factors with levels in a specific order#
filter(gap_minder,year %in% c(1970,2010),country %in% countries, !is.na(per_capita_gdp)) %>% group_by(year) %>% mutate(per_capita_gdp_pd = per_capita_gdp/365, weight = population/sum(population)*2) %>% ungroup() %>% ggplot(aes(x = per_capita_gdp_pd, fill = group, weight = weight)) + geom_density(alpha = 0.2, bw = 0.75, position = "stack") + scale_x_continuous(trans = "log2") + facet_wrap(.~year) #Density plots colour mapped by regions, stacked over each other, each year given weights proportional to the size of the population, and faceted by year#

#Bar plot#
filter(us_contagious_diseases,year == 1967,disease=="Measles",count>0,!is.na(population)) %>% mutate(rate = (count / population) * 10000 * (52 / weeks_reporting),state = reorder(state,rate)) %>% ggplot(aes(x=state,y=rate)) + geom_bar(stat="identity") + coord_flip() #bar plot for disease rates across states for year 1967, with states on x axis and rates on y axis, and coordinate axes flipped to show state names on y axis#

filter(us_contagious_diseases,year == 1967,disease=="Measles",count>0,!is.na(population)) %>% mutate(rate = (count / population) * 10000 * (52 / weeks_reporting),state = reorder(state,rate)) %>% ggplot(aes(x=state,y=rate)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90,hjust = 1)) + xlab("") #bar plot for disease rates across states for year 1967, with states on x axis and rates on y axis#

#Slope chart#
filter(gap_minder,year %in% c(2010,2015),region %in% west,!is.na(life_expectancy),population>10^7) %>% mutate(location = ifelse(year==2010,1,2), location = ifelse(year==2015&country %in% c("Germany","United Kingdom"),location+0.20,location),hjust = ifelse(year==2010,1,0)) %>% ggplot(aes(x=factor(year),y=life_expectancy,group=country,col=country,label=country)) + geom_line(show.legend = F) + geom_text(aes(x=location,hjust=hjust),show.legend = F) + xlab("") #Slope chart for life expectancy of western countries with population > 10 mil, with years as factors on x axis, grouped and colour mapped by country without showing legends, and location of labels adjusted using hjust#

#Bland/Altman plot or Tukey Mean Difference plot#
filter(gap_minder,year %in% c(2010,2015),region %in% west,!is.na(life_expectancy),population>10^7) %>% group_by(country) %>% summarize(average=mean(life_expectancy),difference=life_expectancy[year==2015]-life_expectancy[year==2010]) %>% ggplot(aes(x=average,y=difference,label=country)) + geom_point(size=3) + geom_text_repel(size=6) +xlab("Average of 2010 and 2015") + ylab("Difference between 2015 and 2010") #Altman plot for change in life expectancy between two years, with average on x axis and difference on y axis#

#Coding third variable in plots#
filter(us_contagious_diseases,disease=="Polio",!state %in% c("Hawaii","Alaska")) %>% mutate(rate=count/population*10000) %>% ggplot(aes(x=year,y=state,fill=rate)) + geom_tile(col="grey50") + scale_fill_gradientn(colours = brewer.pal(9,"Reds"), trans="sqrt") + geom_vline(xintercept = 1950, col="blue", size=1.5,) + scale_x_continuous(expand=c(0,0)) + xlab("") + ylab("") + ggtitle("Polio incidence in US states") #Polio incidence rates in US, with states on y axis and years on x axis, rates converted to square root transformation and mapped with sequential colour pallete from brewer, plotted with 'geom_tile' geometry, adding a vertical line at 1950 marking vaccine introduction#

us_avg<-filter(us_contagious_diseases,disease=="Polio",!state %in% c("Hawaii","Alaska"),!is.na(population),!is.na(count)) %>% group_by(year) %>% summarise(us_rate=sum(count)/sum(population)*10000) #creating data frame containing year wise US-average disease rate for polio#
filter(us_contagious_diseases,disease=="Polio",!state %in% c("Hawaii","Alaska"),!is.na(population),!is.na(count)) %>% mutate(rate=count/population*10000) %>% ggplot() + geom_line(mapping=aes(x=year,y=rate,group=state),col="grey50",alpha=0.2,size=1) + geom_line(data=us_avg,mapping=aes(x=year,y=us_rate),col="black",size=1) + geom_vline(xintercept = 1950,size=1.5,col="blue") + scale_y_continuous(trans="sqrt") + ggtitle("Polio incidence in US states") #Polio incidence rates in US, with rates on y axis and years on x axis, scale of y axis converted to square root transformation, with a line showing US-average across years using second 'geom_line' geometry and all mapping done locally#

#Installing and loading packages#
installed.packages() #Generates list of installed packages#
install.package() #Install package from cran#
library() #Load a pre-installed package#

#Downloading, storing and loading data files and objects#
download.file(url = "https://",destfile = "data/xyz.csv") #Downloads file from URL and saves in the 'data' directory#
save(us_contagious_diseases,file="data/us_contagious_diseases.csv") #Saves data file/object from package or global environment to a local directory#
save.image(file="rda/objects.rda") #Saves all objects stored in global environment to 'rda' directory#
load("rda/objects.rda") #Load objects from 'rda' directory into global environment#

#Importing Datasets#
path<-system.file("extdata",package = "dslabs") #Extract full path of a directory in a package#
list.files(path) #List names of files in a directory#
filepath<-file.path(path,"life-expectancy-and-fertility-two-countries-example.csv") #Extract full path of a file/directory in a given path (to a directory)#
getwd() #Print the current working directory#
file.copy(file_path,getwd()) #Copy a file from one path (to a directory) to another path (to a directory)#
raw_data<-read_csv(filepath) #Load a dataset into an object in global environment#

#Packages to be installed#
car
dplyr
dslabs
ggplot2
ggrepel
ggthemes
gridExtra
htmlwidgets
knitr
NHANES
psych
RColorBrewer
readr
readxl
rlang
rmarkdown
rvest
SDSFoundations
stringr
tidyr

#Data Wrangling#
#converting from wide form to tidy form#
tidy_data<-gather(data=wide_data, key=key, value=value, -country) #Gathers data from multiple column headers (all columns except 'country') and puts it in a single column ('key'), also arranges the data values within the columns to a single column ('value')#
tidy_data<-separate(data=tidy_data,col = key,into = c("year","variable"),sep = "_",extra = "merge") #Splits the variables in 'key' column, separated by '_', to multiple columns ('year' and 'variable'),  also merging additional variables separated by '_'#
tidy_data<-spread(data=tidy_data, key=variable, value=value) #Spreads the variable in single column ('variable') to multiple columns (based on number of variables), also arranges data values in 'value' column to within the multiple variable columns#
tidy_data<-mutate(tidy_data,year=as.numeric(year)) #converting the 'year' varibale back to numeric, previously converted to character by gather function#

#combining tables#
murder_polls<-left_join(murder,polls) #Returns a table that combines two data tables by matching rows of a common variable ('state') in first data table ('murder') that are also present in second data table ('polls'), returning 'NA' for rows for which data is absent in the second table#
polls_murder<-right_join(murder,polls) #Returns a table that combines two data tables by matching rows of a common variable ('state') in second data table ('polls') that are also present in first data table ('murder'), returning 'NA' for rows for which data is absent in the first table#
murder_polls_int<-inner_join(murder,polls) #Returns a table that combines two data tables by matching rows of a common variable ('state') that are present in both data tables#
murder_polls_union<-full_join(murder,polls) #Returns a table that combines two data tables by matching rows of a common variable ('state') that are present in either of the two data tables, returning 'NA' for rows for which data is absent in either table#

#filtering tables#
murder_polls_semi<-semi_join(murder,polls) #Returns a table by matching rows of a common variable ('state') in first data table ('murder') that are also present in second data table ('polls'), without copying the variables from the second table#
murder_polls_anti<-anti_join(murder,polls) #Returns a table by matching rows of a common variable ('state') in first data table ('murder') that are not present in second data table ('polls'), without copying the variables from the second table#

#combining vectors/tables using set operators#
intersect(past_country,present_country) #Finds common values between two vectors#
murder_polls_intersect<-intersect(murder[1:5,],murder[3:7,]) #Finds common rows between two tables whose columns match, else returns error highlighting which columns don't match#
murder_polls_union<-union(murder[1:5,],murder[3:7,]) #Finds rows present in either tables and combines them if their columns match, else returns error highlighting which columns don't match#
murder_polls_setdiff<-setdiff(murder[1:5,],murder[3:7,]) #Finds rows present in first table ('murder[1:5]') that are not in second table ('murder[3:7]') if their columns match, else returns error highlighting which columns don't match#
setequal(murder[1:5,],murder[3:7,]) #Tells us if two tables are equal or not regardless of order of rows or columns#

#Web scraping#
webpage_code<-read_html("https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state") #Saves html code from a webpage into an object#
nodes<-html_nodes(webpage_code,"table") #Extracting all nodes of type 'table' from html code into an object#
nodes<-nodes[[2]] #Selecting the required node from all nodes of type 'table'#
webpage_table<-html_table(nodes) #Converting html table into a data frame#

#Renaming variables#
webpage_table<-rename(webpage_table,state="State",population="Population(total inhabitants) (2015) [2]",total="Murders andNonnegligentManslaughter(total deaths) (2015) [1]",murders="Murders(total deaths) (2015) [3]",gun_murders="Gun Murders(total deaths) (2015) [3]",gun_ownership="GunOwnership(%) (2013) [4]",total_rate="Murder andNonnegligentManslaughterRate(per 100,000) (2015)",murder_rate="Murder Rate(per 100,000) (2015)",gun_murder_rate="GunMurder Rate(per 100,000) (2015)") #Renaming variables in a data table#

#String Processing#
a <- '5"' #Define string having single quote within double quotes#
a1 <- "5'" #Define string having double quote within single quotes#
a2 <- "5\"5'" #Define string having both single and double quote by escaping one of the quotes using '\'#
cat(a2) #Print strings without quotation marks#
summarize_all(webpage_table,.funs = function(x) {any(str_detect(string = x,pattern = ","))}) #Detects if each variable in a data table has at least one value with the given pattern#
webpage_table <- mutate_at(webpage_table,2:3,parse_number) #Replaces commas from data values in columns 2 and 3 and convert the values from character to numeric#
not_inches_or_cm<-function(x){inches<-suppressWarnings(as.numeric(x))
index<-!is.na(inches) & ((inches>=50 & inches<=84) | (inches/2.54>=50 & inches/2.54<=84))
!index
} #Defines a function that returns TRUE when a reported value is neither in inches nor in centimters, or if in inches then not between 50 and 84 inches (height range of 99.9999% population); Suppresses Warnings when non-numeric entries are converted to 'NA'#
not_in_inches_or_cm<-filter(rep_heights,not_inches_or_cm(height)) %>% .$height #Filtering reported heights based on 'not_inches_or_cm' function and storing heights variable in an object#
str_view(not_in_inches_or_cm,"ft|foot|feet|inches") #Shows the first instance where a given pattern (Regex) matches in a string, to check whether the defined pattern is working correctly#
str_view_all(not_in_inches_or_cm,"\\d") #Shows all the instances where a given pattern (Regex) matches in a string#
pattern<-"^[4-7]\\s*'\\s*\\d{1,2}$" #Defines a Regex pattern that starts ('^' anchor) with a digit between 4-7 ('[]' character class), then has none or more spaces ('*' quantifier),then has the feet symbol,then none or more spaces ('*' quantifier), and finally ends ('$' anchor) with 1 or 2 digits ('{}' quantifier)#
sum((str_detect(string = not_in_inches_or_cm,pattern))) #Calculates number of height values that match the defined pattern#
str_subset(not_in_inches_or_cm,"inch|feet|foot|ft|''|\\s+|\\.") #Subsets the height values that match the given pattern#
pattern_with_group<-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$" #Defines Regex pattern using groups where group 1 consists of string that starts with one digit b/w 4-7 and group 2 consists of none or more digits at the end, and in between the groups there is either a ',' or '.' or one or more spaces ('+' quantifier)#
str_extract(not_in_inches_or_cm,pattern_with_group) #Extracts strings that match the defined pattern and returns 'NA' for the ones that don't match#
str_match(not_in_inches_or_cm,pattern_with_group) #Extracts the two groups defined in the pattern separately after matching the strings with the defined pattern#
converted<-str_replace(not_in_inches_or_cm,"feet|foot|ft","'") %>% str_replace("inches|in|''|\\|\"","") %>% str_replace(pattern_with_group,"\\1'\\2") #Replaces 'feet,foot,ft' with feet symbol ('), 'inches,in,'',"' by empty character, and 'pattern_with_group' by first group followed by feet symbol followed by second group, then stores results in an object#
index<-str_detect(converted,pattern) #Detects matching patterns in 'converted' vecror, and stores in logical vector#
converted[!index] #Indexes 'converted' vector by 'index' vector to show cases that do not match the defined pattern#
