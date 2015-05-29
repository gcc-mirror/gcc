! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=single" }
!
! CO_BROADCAST
!
program test
  implicit none
  intrinsic co_reduce
  integer :: stat1
  real :: val
  call co_broadcast(val, source_image=1, stat=stat1)
end program test

! { dg-final { scan-tree-dump-times "stat1 = 0;" 1 "original" } }
