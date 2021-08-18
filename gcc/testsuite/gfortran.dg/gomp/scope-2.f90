module m
  implicit none (type, external)
  integer :: r, r2, r3 = 1
  interface
    logical function bar(); end
  end interface
contains

subroutine foo
  integer :: i, j, k
  i = 0; j = 0; k = 0
  !$omp parallel
    if (bar ()) then
        !$omp cancel parallel
    end if
    !$omp scope reduction (+:r) private (i)
      !$omp scope reduction (+:r2) private (j)
        !$omp single
          i = 1;
          j = 2;
          r = r + 1
          r2 = r2 + 1
        !$omp end single nowait
      !$omp end scope
    !$omp end scope
  !$omp end parallel

  !$omp parallel
    if (bar ()) then
        !$omp cancel parallel
    end if
    !$omp scope reduction (task, +:r) private (i)
    !$omp scope reduction (task, *:r3)
      r = r + 1
      r3 = r3 + 1
    !$omp end scope
    !$omp end scope
  !$omp end parallel
end
end module 
