! { dg-do compile }
! { dg-additional-options "-fopenmp -fdump-tree-original" }

! There was a bug in the clause splitting for the "masked taskloop"
! combined directive that caused it to lose all the clauses.

subroutine s1 (a1, a2)
  integer :: a1, a2
  integer :: i, j

  !$omp masked taskloop collapse(2) grainsize(4)
  do i = 1, a1
    do j = 1, a2
    end do
  end do

end subroutine

! { dg-final { scan-tree-dump "omp taskloop \[^\n\r]*grainsize\\(4\\)" "original" } }
! { dg-final { scan-tree-dump "omp taskloop \[^\n\r]*collapse\\(2\\)" "original" } }
