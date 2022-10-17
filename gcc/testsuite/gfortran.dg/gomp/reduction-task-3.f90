! Fortran testcase of reduction-task-3.f90 ( PR c/91149 )

module m
  integer :: r
end

subroutine foo
  use m
  !$omp parallel reduction(task, +: r)
    r = r + 1
  !$omp end parallel
  !$omp target parallel reduction(task, +: r)
    r = r + 1
  !$omp end target parallel
end
