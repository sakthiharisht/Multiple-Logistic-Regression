Train = read.csv("C:/users/sakth/Desktop/BankTrain.csv")
Test = read.csv("C:/users/sakth/Desktop/BankTest.csv")
glm.fit <- glm(y~x1+x2,
               Train,
               family=binomial)
summary(glm.fit)
beta = coef(glm.fit)
plot(Train$x1,
     Train$x2,
     col=Train$y + 1,
     pch=21,
     cex=0.8,
     xlab="x1",
     ylab="x2")
i.val <- c(-15,20)
b.val <- (-beta[1] -beta[3]*i.val)/beta[2]
points(b.val,
       i.val,
       col="blue",
       type="l",
       lwd=2)
legend("bottomright",
       legend = c("Forged", "Genuine"),
       col = c("red","black"),
       pch=21,
       cex = 0.8,
       text.col = "black",
       horiz = FALSE)
glm.probs <- predict(glm.fit,
                     Test,
                     type="response")
glm.pred <- rep(0,nrow(Test))
glm.pred[glm.probs>0.5]=1
table(glm.pred,
      Test$y)
testMSE=mean(glm.pred != Test$y)
testMSE

glm.probs <- predict(glm.fit,
                     Test,
                     type="response")
glm.pred <- rep(0,nrow(Test))
glm.pred[glm.probs>0.3]=1
table(glm.pred,
      Test$y)
testMSE=mean(glm.pred != Test$y)
testMSE

glm.probs <- predict(glm.fit,
                     Test,
                     type="response")
glm.pred <- rep(0,nrow(Test))
glm.pred[glm.probs>0.7]=1
table(glm.pred,
      Test$y)
testMSE=mean(glm.pred != Test$y)
testMSE

library(MASS)
lda.fit=lda(y~x1+x2,
            data=Train)	
lda.pred=predict(lda.fit, 
                 Test)
testMSE=mean(lda.pred$class != Test$y)
testMSE

qda.fit=qda(y~x1+x2,
            data=Train)	
qda.pred=predict(qda.fit,
                 Test)
testMSE=mean(qda.pred$class != Test$y)
testMSE

x = seq(-4,6,length=100)
plot(x,
       0.5*dnorm(x,1,1),
       col="red",
       type="l",
       lwd = 2,
       ylab="pi_k f_k")
points(x,
     0.5*dnorm(x,0,2),
     col="blue",
     type="l",
     lwd = 2)
legend("topright",
       legend = c("Class 0", "Class 1"),
       col = c("blue","red"),
       lwd = 3,
       text.col = "black",
       horiz = FALSE)


