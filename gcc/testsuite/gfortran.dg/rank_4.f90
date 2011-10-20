! { dg-do compile }
! { dg-options "-std=f2008ts -fdump-tree-original" }
!
! PR fortran/48820
!

program test_rank
  implicit none
  intrinsic :: rank

  integer :: a
  real, allocatable :: b(:,:)

  if (rank(a) /= 0) call not_existing()
  if (rank (b) /= 2) call not_existing()
end program test_rank

! { dg-final { scan-tree-dump-times "not_existing" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
