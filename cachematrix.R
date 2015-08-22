## makeCacheMatrix function takes a Matrix input and returns 3 functions setmatrix, getmatrix and get
## setmatrix sets the Inverse of the Matrix, Get takes the matrix input and getmatrix 
## checks the inverse of the input matrix in cache
## called as y <- makeCacheMatrix(matrix(a,b,c))


makeCacheMatrix <- function(x = matrix()) {
m <- NULL
get <- function() x
setmatrix <- function(matrix) m<<- matrix
getmatrix <- function() m
list(setmatrix = setmatrix, getmatrix = getmatrix, get = get)
}


## cacheSolve first checks whether the current value of the Y and 
## it's calculated inverse is available in cache. If so then the cached
## value is returned, otherwise the inverse is calculated and returned

## procedure to call (after makeCacheMatrix):
## x <- matrix(c(1,2,3,2,2,9,7,1,3),3,3)
## y <- makeCacheMatrix(x)
## cacheSolve(y)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setmatrix(m)
        m
}
