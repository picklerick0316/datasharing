# create matrix with all necessary methods
makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    set <- function(y) {
        x <<- y
        matrix <<- NULL
    }
    get <- function() x
    set_matrix <- function(m) matrix <<- m
    get_matrix <- function() matrix
    list(set = set, get = get,
         set_matrix = set_matrix,
         get_matrix = get_matrix)
}

# calculate inverse matrix, if matrix wasn't change - retrieve cached data
cacheSolve <- function(x, ...) {
    matrix <- x$get_matrix()
    print(matrix)
    if(!is.null(matrix)) {
        message("getting cached data")
        return(matrix)
    }
    data <- x$get()
    matrix <- solve(data, ...)
    x$set_matrix(matrix)
    matrix
}
