## Programming assignment 2
## This series of functions will cache the inverse of a matrix in order
## to save computation time each time the inverse is called.
## Functions are based on example given by Prof. Peng for caching the mean of a vector

## This first function creates a list object in which to store the matrix and (after 
## running the next function) its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function to retrieve cached inverse from makeCacheMatrix list object if it exists, 
## otherwiseit calculates the inverse using solve and places it in the matrix list object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

## Output from testing matrices x1 and x2

x1 <- diag(nrow = 2, ncol = 2)
x1
solve(x1)
test1 <- makeCacheMatrix(x1)
cacheSolve(test1)

# > x1 <- diag(nrow = 2, ncol = 2)
# > x1
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > solve(x1)
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > test1 <- makeCacheMatrix(x1)
# > cacheSolve(test1)
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > cacheSolve(test1)
# getting cached data
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1

x2 <- matrix(c(1, 2, 0,
               0, 1, 0,
               0, 0, 2), nrow = 3, ncol = 3)
x2
solve(x2)
test2 <- makeCacheMatrix(x2)
cacheSolve(test2)

# > x2 <- matrix(c(1, 2, 0,
# +               0, 1, 0,
# +               0, 0, 2), nrow = 3, ncol = 3)
# > x2
#      [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    2    1    0
# [3,]    0    0    2
# > solve(x2)
#      [,1] [,2] [,3]
# [1,]    1    0  0.0
# [2,]   -2    1  0.0
# [3,]    0    0  0.5
# > test2 <- makeCacheMatrix(x2)
# > cacheSolve(test2)
#      [,1] [,2] [,3]
# [1,]    1    0  0.0
# [2,]   -2    1  0.0
# [3,]    0    0  0.5
# > cacheSolve(test2)
# getting cached data
#      [,1] [,2] [,3]
# [1,]    1    0  0.0
# [2,]   -2    1  0.0
# [3,]    0    0  0.5

