## "makeCachematrix" allows you to assign the values of a matrix and its inverse, and get these values
## "cacheSolve" checks if the inverse of a matrix is in the cache, and if the matrix has not changed, returns it.
## If the inverse of the matrix is not cache'd, it is calculated and cache'd.

# Let's make a special "inverse" list of four functions that allows you to set the value of a matrix and its inverse, 
# and get these values. 
makeCacheMatrix <- function(x = matrix()) { # Creates a function "makeCacheMatrix" that takes a matrix variable
                                            # "x" as an argument.      
  inv <- NULL                               # Initializes value "inv", the inverse variable, with a NULL value
  
  setfn <- function(y){                       # Creates a function "set" that allows you to assign the argument "y"
    x <<- y                                 # to the matrix x that was defined in the parent scope, *outside* of the local scope, 
                                            # hence the "<<-" operator; if you use the <- operator, it will define another local "x"
    inv <<- NULL                            # Since you assigned a new "x" you need to reinitialize "inv" as NULL to erase any previous values of "inv"
  }
  
  getfn <- function() {return (x)}         # Creates a function "get" returns the matrix "x"
                                            # Note that the function does not take any arguments.
  
  setinversefn <- function(i) {inv <<- i }      # Creates function "setinverse" that allows you to assign the inverse variable "inv"
                                            # takes the argument "i" and assigns it to "inv" defined above
  getinversefn <- function() {return (inv)}           # Creates function "getinverse" that allows one to get the assigned inverse variable
  
  list(set = setfn, get = getfn,                 # Returns a list of our 4 functions  (this allows us to call special$set, special$get, etc.)
       setinverse = setinversefn,              # Assigns "set" function to object name "set", etc....
       getinverse = getinversefn)             
}


# Let's make a cacheSolve function that operates on a "special" inverse object, i.e. the output of makeCacheMatrix.
# It will check if the inverse of a matrix has already been cache'd.  If not, it will solve the inverse of the matrix and cahce's it.

cacheSolve <- function(special, ...) {      # Creates a function cacheSolve that takes "special" as an argument, whichis a makeCacheMatrix "inverse" object 
  ival<- special$getinverse()               # Gets the "inv" value within special and assigns it to the variable "ival."
                                             
  if(!is.null(ival)) {                      # Checks if "ival" is null.  If it's not NULL, return it.
    message("getting cached data")
    return(ival)
  }
                                            # If ival is null, calculate the inverse:
  data <- special$get()                     # Gets the matrix "x" out of "special" and assigns it to the variable "data"
  i <- solve(data, ...)                     # Calls solve() on "data" to get the inverse of x.  Puts the result in the variable "i"
  special$setinverse(i)                     # Pushes the inverse "i" into "special" for future retrieval.  This sets "inv" to "i" so we don't have to calculate "inv" ever again.
  i                                         # Returns the inverse value "i"
}


