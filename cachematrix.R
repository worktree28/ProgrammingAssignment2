## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL          ## initialize inv as NULL; will hold value of inverse matrix  
      set <- function(y) { 
            x <<- y        ## value of matrix in parent environment
            inv <<- NULL   
      }
      get <- function() x 
      setInverse <- function(inverse) {inv <<- inverse} 
      getInverse <- function() inv  
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute inv either by retrieving it from cache or computing it

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)) {    
            message("getting cached data") 
            return(inv)
      }
      m <- x$get()
      inv <- solve(m, ...)
      x$setInverse(inv)
      inv
}
