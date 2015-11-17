# Programming Assignment 2 -- Caching Inverse Matrix
## makeCacheMatrix -- Create special matrix that stores its inverse
## cacheSolve <-- Determine if inverse has already been calculated (if not, solve)

## FUNCTION: makeCacheMatrix -- Create "special" matrix that stores its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL # Initialize "inv"
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # FUNCTION: Set -- Set the value of the stored matrix
  
  get <- function() x
  # FUNCTION: Get -- Get value of the stored matrix
  
  setinv <- function(inverse) inv <<- inverse
  # FUNCTION: setinv -- set the value of the inverse
  
  getinv <- function() inv
  # FUNCTION getinv -- get the value of the inverse
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  # Return a list 
}


## FUNCTION: cacheSolve -- test to see if inverse exists (if not, solve)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    # If the inverse already exists. . . 
    
    message("getting cached inverse")
    return(inv)
    
  }
  
  data <- x$get()
  # Get the matrix stored in x
  
  xinv <- solve(data, ...)
  # Calculate the inverse with solve
  
  x$setinv(xinv)
  # Set the inverse in X
  
  xinv
  # Return the calculated (or cached) inverse
}
