## function makeCacheMatrix creates a matrix object
## and defines four functions for it.
## input: vector object
## output: list object with four functions:
## set,get,set operator, get operator
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setoper <- function(oper) m <<- oper
    getoper <- function() m
    list(set = set, get = get,
         setoper = setoper,
         getoper = getoper)
}

## Function casheSolve 
## Return a matrix that is the inverse of 'x'
## A check is performed if the matrix is squared.
## A check is not performed if matrix is changed - it is controlled by call of makeCacheMatrix.
## A check is not performed if matrix is inversible - from the text of the assignment.
cacheSolve <- function(x, ...) {
    m <- x$getoper()
    v <- x$
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    data_dim<-dim(data)
    if(data_dim[1]!=data_dim[2]) {
      message("error in casheSolve: non-squared matrix")
      return(0)
    }
    m <- solve(data, ...)
    x$setoper(m)
    m
}
