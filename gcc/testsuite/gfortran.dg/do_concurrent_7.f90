! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
! PR fortran/113305

program dc
  implicit none
  real :: a(12), b(12), c(16,8), d(16,8)
  integer :: i, j
  call random_number(b)
!GCC$ ivdep
!GCC$ vector
  do concurrent (i=1:12)
     a(i) = 2*b(i)
  end do
  c = b(1)
  d = a(2)
!GCC$ novector
!GCC$ unroll 4
  do concurrent (i=1:16:2,j=1:8:2)
     d(i,j) = 3*c(i,j)
  end do
end program

! { dg-final { scan-tree-dump "ANNOTATE_EXPR .* ivdep>, vector" "original" } }
! { dg-final { scan-tree-dump "ANNOTATE_EXPR .* ivdep>, no-vector" "original" } }
! { dg-final { scan-tree-dump "ANNOTATE_EXPR .* ivdep>, unroll 4>, no-vector" "original" } }
