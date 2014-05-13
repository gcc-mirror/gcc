! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=single" }
!
! CO_SUM/CO_MIN/CO_MAX
!
program test
  implicit none
  intrinsic co_max
  integer :: stat1, stat2, stat3
  real :: val
  call co_max(val, stat=stat1)
  call co_min(val, stat=stat2)
  call co_sum(val, stat=stat3)
end program test

! { dg-final { scan-tree-dump-times "stat1 = 0;" 1 "original" } }
! { dg-final { scan-tree-dump-times "stat2 = 0;" 1 "original" } }
! { dg-final { scan-tree-dump-times "stat3 = 0;" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }

