## 1 set a function that will create a matrix
## functions do

## The initial function will define the functions that will return the inverse of he matrix

makeCacheMatrix <- function(x = matrix()) {invn <- NULL
    	set <- function(y) {
        x <<- y
        invn <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invn <<- inverse
    getinverse <- function() invn
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cache solve will return the inverse of the matrix if doesn't already exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
invn <- x$getinverse()
    if(!is.null(invn)) {
        message("getting cached data.")
        return(invn)
    }
    data <- x$get()
    invn <- solve(data)
    x$setinverse(invn)
    invn}
