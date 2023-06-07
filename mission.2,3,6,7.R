#Exp-3
#correlation and regression
data=cars 
data
summary(data)
v1=var(data$speed) 
v1
v2=var(data$dist) 
v2
covariance=cov(data$speed,data$dist) 
covariance
corr=covariance/(sd(data$speed)*sd(data$dist)) 
corr
cor.test(data$speed,data$dist)
cor.test(data$speed,data$dist,method=”pearson”)
cor.test(data$speed,data$dist,method="spearman")
plot(data$speed,data$dist)
regression1=lm(data$speed~data$dist) 
regression1
abline(regression1) 
summary(regression1)
regression2=lm(data$dist~data$speed) 
regression2
abline(regression2)
summary(regression2)
#-----------------------------------------------------
##Exp-2
#plotting visualising data and tabulation
empid=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15) 
empid
age=c(30,37,45,32,50,60,35,32,34,43,32,30,43,50,60) 
age
gender=c(0,1,0,1,1,0,0,1,0,0,1,1,0,0) 
gender
status=c(1,1,2,2,1,1,1,2,2,1,2,1,2,1,2) 
status
empinfo=data.frame(empid,age,gender,status) 
empinfo
empinfo$gender=factor(empinfo$gender,labels=c("male","female")) 
empinfo$gender
empinfo$status=factor(empinfo$status,labels=c("staff","faculty")) 
empinfo$status
empinfo
male=subset(empinfo,empinfo$gender=="male") 
male
female=subset(empinfo, empinfo$gender=='female') 
female
summary(empinfo)
summary(male)
summary(female)
summary(age)
table1=table(empinfo$gender)
 table1
table2=table(empinfo$status) 
table2
table3=table(empinfo$gender, empinfo$status) 
table3
plot( empinfo$age , type="l", main="Age of employees", xlab="empid", ylab="age in years", col="blue")
pie(table1)
barplot( table3 , beside=T ,xlim=c(1,15), ylim=c(0,5), col=c("blue", "red")) legend("topright",legend=rownames(table3),fill=c('blue','red'), bty="n")
boxplot(empinfo$age~empinfo$status,col=c('red','blue'))



problem-2
weight=c(15,26,27,2,25.5,27,32,18,22,20,26,24)
weight
bmi=c(133.35,16.1,16.74,16,13.59,15.73,15.65,13.85,16.07,12.8,13.65,14.42)
bmi
cor(weight,bmi)
mode1<-lm(bmi~weight) 
summary.lm(mode1)
#----------------------------------------------------------------------------------
Exp-6
#poissson
m=20
m
ps=0.02
lambda=m*ps
lambda
p1=sum(dpois(2:m,lambda))
p1
round(1000*p1)
p2=dpois(2,lambda)
p2
round(1000*p2)
p3=sum(dpois(0:2, lambda))
p3
round(1000*p3)
x1=0:m
px1=dpois(x1,lambda)
plot(x1,px1,type="h",xlab="values of x",ylab="Probability distribution of 
x",main="Poisson distribution")
Ex1=weighted.mean(x1,px1)
Ex1
Varx1=weighted.mean(x1*x1,px1)-(weighted.mean(x1 ,px1))^2
Varx1
#---------------------------------------------------------------------------------------
#normal distribution
x=seq(0,40)
x
y=dnorm(x,mean=20,sd=5)
y
plot(x,y,type='l')
p1=pnorm(15,mean=20,sd=5)
p1
x2=seq(0,15)
x2
y2=dnorm(x2,mean=20,sd=5)
y2
p2=pnorm(40,mean=20,sd=5)-pnorm(25,mean=20,sd=5)
p2
x1=seq(25,40)
x1
y1=dnorm(x1,mean=20,sd=5)
y1
polygon(c(25,x1,40),c(0,y1,0),col='red')
p3=pnorm(25,mean=20,sd=5)-pnorm(15,mean=20,sd=5)
p3
x3=seq(15,25)
x3
y3=dnorm(x3,mean=20,sd=5)
y3
polygon(c(15,x3,25),c(0,y3,0),col='green')
data.frame(p1,p2,p3)
------------------------------------------------------------------------------------------------
EXP-7-large sample mean test
xbar=14.6
xbar
mu0=15.4
mu0
sigma=2.5
sigma
n=35
n
z=(xbar-mu0)/(sigma/sqrt(n))
z
alpha=0.05
alpha
zhalfalpha=qnorm(1-(alpha/2))
zhalfalpha
c(-zhalfalpha,zhalfalpha)
pval=2*pnorm(z)
pval
if(pval>alpha){print("Accept Null hypothesis")} else{print("Reject Null 
hypothesis")}
#----------------------------------------------------------------------------------------------------
large sample proportion test
n=640
n
Sprop=63/n
Sprop
Pprop=0.1726
Pprop
q=1-Pprop
q
z=(Sprop-Pprop)/sqrt(Pprop*q/n)
z
E=qnorm(.975)
c(-E,E)
Sprop+c(-E,E)*sqrt(Pprop*(1-Pprop)/n)
if(z>-E && z<E){print("Hospital is not efficient")} else{print("Hospital is 
efficient")}
------------------------------------------------------------------------------------------------------------
Exp--8
Two sample mean test
xbar=20
xbar
ybar=15
ybar
sigma=4
sigma
n1=500
n1
n2=400
n2
z=(xbar-ybar)/(sigma*sqrt((1/n1)+(1/n2)))
z
alpha=0.05
alpha
zalpha=qnorm(1-(alpha/2))
zalpha
if(z<=zalpha){print("Accept Null hypothesis")} else{print("Reject Null 
hypothesis")}
#-----------------------------------------------------------------------------------------------------------
Two sample proportion test
p1=0.20
p1
p2=0.185
p2
n1=900
n1
n2=1600
n2
P=(n1*p1+n2*p2)/(n1+n2)
P
Q=1-P
# Test Statistic
z=(p1-p2)/sqrt(P*Q*sqrt((1/n1)+(1/n2)))
z
alpha=0.05
alpha
zalpha=qnorm(1-(alpha/2))
zalpha
if(z<=zalpha){print("Accept Null hypothesis")} else{print("Reject Null 
hypothesis")}
-----------------------------------------------------------------------------------------------------------
Exp-9
> #Two indep sample sizes of size 8 and 8 contained the following values
> #sample 1 - 19 - 17 - 15 - 21 - 16 - 18 - 16 - 14
> #sample 2 - 15 - 14 - 15 - 19 - 15 - 18 - 16 - 20
> #Is the difference btw sample means significant ?
> #input the data
> sample1 = c(19,17,15,21,16,18,16,14)
> sample2 = c(15,14,15,19,15,18,16,20)
> sample1
[1] 19 17 15 21 16 18 16 14
> sample2
[1] 15 14 15 19 15 18 16 20
> #output using t-distribution
> t=t.test(sample1,sample2)
> t

	Welch Two Sample t-test

data:  sample1 and sample2
t = 0.44721, df = 13.989, p-value = 0.6616
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -1.898128  2.898128
sample estimates:
mean of x mean of y 
     17.0      16.5 

> #test-statistics
> cv = t$statistic
> cv
        t 
0.4472136 
> #Critical value
> tv = qt(0.975,14)
> tv
[1] 2.144787
> #conclusion
> if(cv<=tv){print("Accept H0")} else{print("Reject H0")}
[1] "Accept H0"



