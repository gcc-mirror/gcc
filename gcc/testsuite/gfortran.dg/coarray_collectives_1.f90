! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
!
! CO_SUM/CO_MIN/CO_MAX
!
program test
  implicit none
  intrinsic co_max
  intrinsic co_min
  intrinsic co_sum
  integer :: val, i
  character(len=30) :: errmsg
  integer(8) :: i8
  character(len=19, kind=4) :: msg4

  call co_sum("abc") ! { dg-error "must have a numeric type" }
  call co_max(cmplx(1.0,0.0)) ! { dg-error "shall be of type integer, real or character" }
  call co_min(cmplx(0.0,1.0)) ! { dg-error "shall be of type integer, real or character" }

  call co_sum(1) ! { dg-error "must be a variable" }
  call co_min("abc") ! { dg-error "must be a variable" }
  call co_max(2.3) ! { dg-error "must be a variable" }

  call co_sum(val, result_image=[1,2]) ! { dg-error "must be a scalar" }
  call co_sum(val, result_image=1.0) ! { dg-error "must be INTEGER" }
  call co_min(val, stat=[1,2]) ! { dg-error "must be a scalar" }
  call co_min(val, stat=1.0) ! { dg-error "must be INTEGER" }
  call co_min(val, stat=1) ! { dg-error "must be a variable" }
  call co_min(val, stat=i, result_image=1) ! OK
  call co_max(val, stat=i, errmsg=errmsg, result_image=1) ! OK
  call co_max(val, stat=i, errmsg=[errmsg], result_image=1) ! { dg-error "must be a scalar" }
  call co_max(val, stat=i, errmsg=5, result_image=1) ! { dg-error "must be CHARACTER" }
  call co_sum(val, errmsg="abc") ! { dg-error "must be a variable" }

  call co_sum(val, stat=i8) ! { dg-error "The stat= argument at .1. must be a kind=4 integer variable" }
  call co_min(val, errmsg=msg4) ! { dg-error "The errmsg= argument at .1. must be a default-kind character variable" }
end program test
