! { dg-options "-O3 -fdump-tree-lversion-details -fno-frontend-loop-interchange" }

! Test a case in which the outer loop iterates over the inner dimension.
! The options above prevent the frontend from interchanging the loops.

subroutine f1(x, limit, step, n)
  integer :: limit, step, n
  real :: x(limit, n)
  do i = 1, limit, step
     do j = 1, n
        x(i, j) = 100
     end do
  end do
end subroutine f1

subroutine f2(x, n, limit, step)
  integer :: n, limit, step
  real :: x(limit, n)
  do i = 1, n
     do j = 1, limit, step
        x(j, i) = 100
     end do
  end do
end subroutine f2

! FIXME: The frontend doesn't give us enough information to tell which loop
! is iterating over the innermost dimension, so we optimistically
! assume the inner one is.
! { dg-final { scan-tree-dump-not {want to version} "lversion" { xfail *-*-* } } }
! { dg-final { scan-tree-dump-not {versioned} "lversion" { xfail *-*-* } } }
