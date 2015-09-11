! { dg-do compile }
! { dg-options "-fdump-tree-original -fcoarray=single" }
!
! CO_REDUCE
!
program test
  implicit none
  intrinsic co_reduce
  integer :: stat1
  real :: val
  call co_reduce(val, valid, result_image=1, stat=stat1)
contains
  pure real function valid(x,y)
    real, value :: x, y
    valid = x * y
  end function valid
end program test

! { dg-final { scan-tree-dump-times "stat1 = 0;" 1 "original" } }
