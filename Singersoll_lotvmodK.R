# Lotka-Volterra Model with Carrying Capacity and Hunting
#' function computes the rate of change of populations in a predator-prey interaction with hunting
#' @param t time (days)
#' @param pop list initial conditions; list with two values prey=number of prey and pred=number of predator
#' @param pars list coefficient in Lotka-Volterra pars$rprey, pars$alpha, pars$eff, pars$pmort, pars$hunt_rate, pars$min_prey, pars$K
#' \emph{rprey} is the growth rate of prey population;
#' \emph{eff} is the rate of ingestion of prey by predators;
#' \emph{alpha} is an interaction coefficient (higher values mean greater interaction);
#' \emph{pmort} is the mortality rate of the predator population;
#' \emph{hunt_rate} is the rate at which prey are hunted;
#' \emph{min_prey} is the minimum prey population required for hunting to occur;
#' \emph{K} is the carrying capacity of the environment.
#' @examples
#' lotvmodK(t=1, pop=list(prey=1, pred=2), pars=list(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2, hunt_rate=0.1, min_prey=5, K=100))
#'
#' pars = list(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2, hunt_rate=0.1, min_prey=5, K=100)
#' currpop = c(prey=50, pred=5)
#' days = seq(from=1, to=200)
#' res = ode(func=lotvmodK, y=currpop, times=days, parms=pars)
#' 
# Lotka-Volterra Model with Carrying Capacity and Hunting
lotvmodK = function(t, pop, pars) {
  
  with(as.list(c(pars, pop)), {
    # Check if the prey population is above the minimum threshold
    if (prey > min_prey) {
      # Calculate the number of prey hunted, ensuring it doesn't reduce the population below min_prey
      hunted_prey = min(hunt_rate * prey, prey - min_prey)
    } 
    
    else {
      # No hunting if prey population is below or equal to min_prey
      hunted_prey = 0  
    }
    
    # Calculate the rate of change of prey population
    dprey = rprey * (1 - prey / K) * prey - alpha * prey * pred - hunted_prey
    
    # Calculate the rate of change of predator population
    dpred = eff * alpha * prey * pred - pmort * pred
    
    # Return the rates of change
    return(list(c(dprey, dpred)))
  })
}



