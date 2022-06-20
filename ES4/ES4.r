## Econometrics 2
## ES4

# Install packages if necessary

install.packages("plm")
install.packages("texreg")

rm(list = ls()) 

# load required packages
library(plm)
library("texreg")

# load data
data("EmplUK", package = "plm")


# Arellano-Bond

a1 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
          + lag(log(capital), 0:2)+lag(log(output), 0:2) | lag(log(emp), 2:9),
          data = EmplUK, effect = "twoways", model = "onestep")


a1_onestep <- pgmm(log(emp) ~ lag(log(emp), 1) + lag(log(cap), 0:1)
                   +lag(log(indoutpt), 0:1)+lag(log(wage), 0:1) | lag(log(emp), 1:9) + diff(log(cap))+diff(log(wage))+diff(log(indoutpt)),
                   data = EmplUK, effect = "twoways", model = "onestep")



a2 <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + lag(log(capital), 0:2)+lag(log(output), 0:2) | lag(log(emp), 2:9),
           data = EmplUK, effect = "twoways", model = "twosteps")


b <- pgmm(log(emp) ~ lag(log(emp), 1:2) + lag(log(wage), 0:1)
           + log(capital)+lag(log(output), 0:1) | lag(log(emp), 2:9),
           data = EmplUK, effect = "twoways", model = "twosteps")


screenreg(list(a1 = a1, a2 = a2, b = b),
          digits = 3, omit.coef = "(Intercept)")


# Anderson-Hsiao

AH <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
             lag(log(capital), 0:1) | lag(log(emp), 2),        
           data = EmplUK, effect = "twoways", model = "onestep")

summary(AH)

# Blundell-Bond


# Arellano-Bond one-step estimator with predetermined regressors
GMM_DIF <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
             lag(log(capital), 0:1) | lag(log(emp), 2:9) +
             lag(log(wage), 2:9) + lag(log(capital), 2:9),        
           data = EmplUK, effect = "twoways", model = "onestep")
summary(GMM_DIF)


# Blundell-Bond one-step systems GMM estimator
GMM_SYS <- pgmm(log(emp) ~ lag(log(emp), 1)+ lag(log(wage), 0:1) +
                        lag(log(capital), 0:1) | lag(log(emp), 2:9) +
                        lag(log(wage), 2:9) + lag(log(capital), 2:9),        
                      data = EmplUK, effect = "twoways", model = "onestep", 
                      transformation = "ld")

summary(GMM_SYS, robust = TRUE)

# end
