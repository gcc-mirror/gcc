subroutine foo
  integer :: n = 5, m = 7
  !$omp declare target to(n) ! { dg-warning "'to' clause with 'declare target' at \\(1\\) deprecated since OpenMP 5.2, use 'enter' \\\[-Wdeprecated-openmp\\\]" }
  !$omp threadprivate (m)
end

program main
  integer :: i, j
  !$omp declare target to(i) ! { dg-warning "'to' clause with 'declare target' at \\(1\\) deprecated since OpenMP 5.2, use 'enter' \\\[-Wdeprecated-openmp\\\]" }
  !$omp threadprivate (j)
end
