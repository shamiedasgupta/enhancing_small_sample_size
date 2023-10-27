
library(MKinfer)
library(EnvStats)
library(fGarch)
library(clipr)

fun_norm=function(n,th,th0,sk)
{
dat=rsnorm(n,mean=th,sd=2,xi=sk)

test.p=oneSamplePermutationTest(dat,mu=th0,alternative='greater',exact=TRUE)

test.b=boot.t.test(dat,mu=th0,alternative='greater',R=1000)

test.w=wilcox.test(dat,mu=th0,alternative="greater",exact=TRUE)

return(c(test.p$p.value,test.b$boot.p.value,test.w$p.value))
}

set.seed(1)
fun_norm(8,7,5,1)

R=1000
fun_pow=function(n,th,th0,sk)
{

pow_p=pow_b=pow_w=p_p=p_b=p_w=0
for(i in 1:R)
{
  f=fun_norm(n,th,th0,sk)
  p_p[i]=f[1]
  p_b[i]=f[2]
  p_w[i]=f[3]
}

return(c(length(p_p[p_p<0.05])/R,
length(p_b[p_b<0.05])/R,
length(p_w[p_w<0.05])/R))
}

d=data.frame()
for (n in c(3,4,5,6,7,8,9,10,12))
{
  d=rbind(d,fun_pow(n,9,5,0.8))
}

colnames(d)=c("PT","BT","WT")
d

fun_pow(11,9,5,0.8)
write_clip(d)

#__________________________________

fun_logis=function(n,th,th0)
{
  dat=rlogis(n,location=th,scale=2)
  
  test.p=oneSamplePermutationTest(dat,mu=th0,alternative='greater',exact=TRUE)
  
  test.b=boot.t.test(dat,mu=th0,alternative='greater',R=1000)
  
  test.w=wilcox.test(dat,mu=th0,alternative="greater",exact=TRUE)
  
  return(c(test.p$p.value,test.b$boot.p.value,test.w$p.value))
}

set.seed(1)
fun_logis(8,5,5)

R=1000
fun_pow1=function(n,th,th0)
{
  
  pow_p=pow_b=pow_w=p_p=p_b=p_w=0
  for(i in 1:R)
  {
    f=fun_logis(n,th,th0)
    p_p[i]=f[1]
    p_b[i]=f[2]
    p_w[i]=f[3]
  }
  
  return(c(length(p_p[p_p<0.05])/R,
           length(p_b[p_b<0.05])/R,
           length(p_w[p_w<0.05])/R))
}

d=data.frame()
for (n in c(3,4,5,6,7,8,9,10,12))
{
  d=rbind(d,fun_pow1(n,9,5))
}

colnames(d)=c("PT","BT","WT")
d

fun_pow1(11,9,5)
write_clip(d)

#____________________________________________

fun_cauch=function(n,th,th0)
{
  dat=rcauchy(n,location=th,scale=2)
  
  test.p=oneSamplePermutationTest(dat,mu=th0,alternative='greater',exact=TRUE)
  
  test.b=boot.t.test(dat,mu=th0,alternative='greater',R=1000)
  
  test.w=wilcox.test(dat,mu=th0,alternative="greater",exact=TRUE)
  
  return(c(test.p$p.value,test.b$boot.p.value,test.w$p.value))
}

set.seed(1)
fun_cauch(8,5,5)

R=1000
fun_pow2=function(n,th,th0)
{
  
  pow_p=pow_b=pow_w=p_p=p_b=p_w=0
  for(i in 1:R)
  {
    f=fun_cauch(n,th,th0)
    p_p[i]=f[1]
    p_b[i]=f[2]
    p_w[i]=f[3]
  }
  
  return(c(length(p_p[p_p<0.05])/R,
           length(p_b[p_b<0.05])/R,
           length(p_w[p_w<0.05])/R))
}

d=data.frame()
for (n in c(3,4,5,6,7,8,9,10,12))
{
  d=rbind(d,fun_pow2(n,9,5))
}

colnames(d)=c("PT","BT","WT")
d

fun_pow2(n,9,5)
write_clip(d)



#__________________________________

sp=c(28,-44,29,30,26,27,22,23,33,
     16,24,4,21,31,34,-2,25,19)
length(sp)

qqnorm(sp)
library(car)
shapiro.test(sp)
qqPlot(sp)



test.p=oneSamplePermutationTest(sp,mu=33.02,alternative='two.sided',exact=TRUE)

test.b=boot.t.test(sp,mu=33.02,alternative='two.sided',R=1000)

test.w=wilcox.test(sp,mu=33.02,alternative='two.sided',exact=TRUE)

test.p$p.value
test.b$p.value
test.w$p.value

