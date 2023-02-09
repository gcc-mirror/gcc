! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

program main
  implicit none
  integer, parameter :: N = 1000
  integer :: a(N), b(N), i

  ! Should be able to parse present in to/from clauses of 'target update'.
  !$omp target update to(present: a) from(present: b)
end program

! { dg-final { scan-tree-dump "pragma omp target update to\\(present:a \\\[len: \[0-9\]+\\\]\\) from\\(present:b \\\[len: \[0-9\]+\\\]\\)" "gimple" } }
