## The overall purpose of this script is to make use of R's caching function
## to store, if wanted, the inverse of a square matrix, 'x'.
## NB matrix 'x' must be invertible


## This function creates an object of the makeMatrix type, which we custom-define.
## A makeVector object is a list which has four elements, each of which is a function
##

makeMatrix <- function(x = matrix()) {
  m <- NULL #initialise variable in makeMatrix environment (i.e., parent of getter/setters below)
  set <- function(y) {
    x <<- y     #set the x value in parent (makeMatrix) environment so is available for lower functions later
    m <<-NULL   #clears value of m in parent environment
  }
  get <- function() x  #returns x value from parent environment of makeMatrix - using the cache
  setinverse <- function(inv) m <<- inv  #sets m in parent environment using child function 
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## This function take as an input an object which is of the makeMatrix type. 
## It then uses the functions of the makeMatrix type to return the inverse of the original 
## matrix, x, passed to the makeMatrix function. 
## This is possible because of lexical scoping - the original matrix x 
## is in the makeMatrix object environment because that object has getter/setter functions
## the keep the original x and m in memory. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', but only if no inverse value has been calculated
  
  m <- x$getinverse()
  if(!is.null(m)) {   #tests if an object m exists already
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


