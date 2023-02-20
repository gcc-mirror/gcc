! { dg-options "-fno-openmp -fopenmp-simd -fdump-tree-original" }

! While 'omp assumes' is ignored with -fopenmp-simd,
! 'omp assume' is processed - check that this works.

module m
  !$omp assumes no_openmp invalid_clause  ! Should get ignored
contains
  integer function foo()
    foo = 5
  end function
end

program main
  use m
  implicit none
  !$omp assumes no_openmp  ! likewise ignored
  integer :: n
  !$omp assume holds (foo() > 0) ! should be honoured
    n = foo()
    if (n == 0) stop
  !$omp end assume
end

! { dg-final { scan-tree-dump "\\.ASSUME \\(foo \\(\\) > 0\\);" "original" } }
