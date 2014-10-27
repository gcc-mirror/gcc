! { dg-do compile }
! { dg-options "-fcoarray=single -fmax-errors=40" }
!
!
! CO_BROADCAST/CO_REDUCE
!
program test
  implicit none
  intrinsic co_broadcast
  intrinsic co_reduce
  integer :: val, i
  integer :: vec(3), idx(3)
  character(len=30) :: errmsg
  integer(8) :: i8
  character(len=19, kind=4) :: msg4

  interface
    pure function red_f(a, b)
      integer :: a, b, red_f
      intent(in) :: a, b
    end function red_f
    impure function red_f2(a, b)
      integer :: a, b, red_f
      intent(in) :: a, b
    end function red_f2
  end interface

  call co_broadcast("abc") ! { dg-error "Missing actual argument 'source_image' in call to 'co_broadcast'" }
  call co_reduce("abc") ! { dg-error "Missing actual argument 'operator' in call to 'co_reduce'" }
  call co_broadcast(1, source_image=1) ! { dg-error "'a' argument of 'co_broadcast' intrinsic at .1. must be a variable" }
  call co_reduce(a=1, operator=red_f) ! { dg-error "'a' argument of 'co_reduce' intrinsic at .1. must be a variable" }
  call co_reduce(a=val, operator=red_f2) ! { dg-error "OPERATOR argument at \\(1\\) must be a PURE function" }

  call co_broadcast(val, source_image=[1,2]) ! { dg-error "must be a scalar" }
  call co_broadcast(val, source_image=1.0) ! { dg-error "must be INTEGER" }
  call co_broadcast(val, 1, stat=[1,2]) ! { dg-error "must be a scalar" }
  call co_broadcast(val, 1, stat=1.0) ! { dg-error "must be INTEGER" }
  call co_broadcast(val, 1, stat=1) ! { dg-error "must be a variable" }
  call co_broadcast(val, stat=i, source_image=1) ! OK
  call co_broadcast(val, stat=i, errmsg=errmsg, source_image=1) ! OK
  call co_broadcast(val, stat=i, errmsg=[errmsg], source_image=1) ! { dg-error "must be a scalar" }
  call co_broadcast(val, stat=i, errmsg=5, source_image=1) ! { dg-error "must be CHARACTER" }
  call co_broadcast(val, 1, errmsg="abc") ! { dg-error "must be a variable" }
  call co_broadcast(val, 1, stat=i8) ! { dg-error "The stat= argument at .1. must be a kind=4 integer variable" }
  call co_broadcast(val, 1, errmsg=msg4) ! { dg-error "The errmsg= argument at .1. must be a default-kind character variable" }

  call co_reduce(val, red_f, result_image=[1,2]) ! { dg-error "must be a scalar" }
  call co_reduce(val, red_f, result_image=1.0) ! { dg-error "must be INTEGER" }
  call co_reduce(val, red_f, stat=[1,2]) ! { dg-error "must be a scalar" }
  call co_reduce(val, red_f, stat=1.0) ! { dg-error "must be INTEGER" }
  call co_reduce(val, red_f, stat=1) ! { dg-error "must be a variable" }
  call co_reduce(val, red_f, stat=i, result_image=1) ! OK
  call co_reduce(val, red_f, stat=i, errmsg=errmsg, result_image=1) ! OK
  call co_reduce(val, red_f, stat=i, errmsg=[errmsg], result_image=1) ! { dg-error "must be a scalar" }
  call co_reduce(val, red_f, stat=i, errmsg=5, result_image=1) ! { dg-error "must be CHARACTER" }
  call co_reduce(val, red_f, errmsg="abc") ! { dg-error "must be a variable" }
  call co_reduce(val, red_f, stat=i8) ! { dg-error "The stat= argument at .1. must be a kind=4 integer variable" }
  call co_reduce(val, red_f, errmsg=msg4) ! { dg-error "The errmsg= argument at .1. must be a default-kind character variable" }

  call co_broadcast(vec(idx), 1) ! { dg-error "Argument 'A' with INTENT\\(INOUT\\) at .1. of the intrinsic subroutine co_broadcast shall not have a vector subscript" }
  call co_reduce(vec([1,3,2]), red_f) ! { dg-error "Argument 'A' with INTENT\\(INOUT\\) at .1. of the intrinsic subroutine co_reduce shall not have a vector subscript" }
end program test
