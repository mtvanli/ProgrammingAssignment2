## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix", which 
## returns a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- matrix(0,nrow=nrow(x),ncol=ncol(x))
    set <- function(y){
        x <<- as.matrix(y)
        i <<- matrix(0,nrow=nrow(x),ncol=ncol(x))
    }
    get <-function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
    
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve retrieves the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!all(i==0)){
        message("getting cahced data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}