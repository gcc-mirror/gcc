module m
  implicit none (external, type)
  integer :: r, r2, r3
contains

subroutine foo
  integer :: i, j, k
  i = 0; j = 0; k = 0
  !$omp scope private (i) reduction (+:r)
    i = 1
    r = r + 1
  !$omp end scope nowait

  !$omp scope private (i) reduction (task, +:r)
  !$omp scope private (j) reduction (task, +:r2)
  !$omp scope private (k) reduction (task, +:r3)
    i = 1
    j = 2
    k = 3
    r = r + 1
    r2 = r2 + 1
    r3 = r3 + 1
  !$omp end scope
  !$omp end scope
  !$omp end scope
  !$omp parallel
    !$omp scope reduction (+:r) private (i)
      !$omp scope reduction (+:r2) private (j)
        !$omp single
          i = 1
          j = 2
          r = r + 1
          r2 = r2 + 1
        !$omp end single
      !$omp end scope nowait
    !$omp end scope nowait
  !$omp end parallel
end
end module
