subroutine foo
  integer :: n = 5, m = 7
  !$omp declare target to(n)
  !$omp threadprivate (m)
end

program main
  integer :: i, j
  !$omp declare target to(i)
  !$omp threadprivate (j)
end
