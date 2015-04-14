## makeCacheMatrix creates a matrix out of xm 
## creates a function that sets the inverted matrix
## gets it and stores it
## cacheSolve checks if the inverted matrix is in cache
## if not, it calculates this and else gets it from cache
## test it by typing: 
## x = matrix(1:4,2,2)
## test = makeCacheMatrix(x)
## cacheSolve(test)

makeCacheMatrix = function(xm = matrix()) {
        xinv = NULL
        set = function(y) {
                xm <<- y
                xinv <<- NULL
        }
        get <- function() xm
        setinv <- function(solve) xinv <<- solve
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(xm, ...) {
        xinv <- xm$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- xm$get()
        xinv <- solve(data, ...)
        xm$setinv(xinv)
        xinv
}
