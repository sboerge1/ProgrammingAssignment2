## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly.
## This pair of functions will cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

##sample return:

##creating a "special" matrix that cache its inverse
## x=rbind(c(2,-1), c(-1,2))
##w=makeCacheMatrix(x)
##w$get()
##     [,1] [,2]
##[1,]    2   -1
##[2,]   -1    2

##Computing the inverse of the "special" matrix created above
##cacheSolve(w)
##[,1]      [,2]
##[1,] 0.6666667 0.3333333
##[2,] 0.3333333 0.6666667

#Computing again, but this time retrieving from the cached data
## cacheSolve(w)
##getting cached data.
##[,1]      [,2]
##[1,] 0.6666667 0.3333333
##[2,] 0.3333333 0.6666667