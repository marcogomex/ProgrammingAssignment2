## This function calculates the inverse of a given matrix.

## The inverse matrix calculations use the solve method and
## a cached value to acelerate the calculations when available.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  
  setMatrix <- function(y){
    x <<- y
    invMatrix <- NULL
  }
  
  getMatrix <- function() x
  
  getInverse <- function() {
    if(is.null(invMatrix)){      
      invMatrix <<- solve(x)
    }else{
      message("Fetching from cache!")  
    }
    
    invMatrix
  }
  
list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse)
}


## Resolves the cache value of inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  invMatrix
}
