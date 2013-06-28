! { dg-do compile }
!
! PR fortran/57035
!
!

subroutine assumed_rank (a)
  use iso_c_binding
  integer, intent(in), target :: a(..)
  integer :: c(1:4)
  type(c_ptr) :: xx
  c = ubound(c,a) ! { dg-error "Assumed-rank argument at .1. is only permitted as first actual argument to the intrinsic inquiry function ubound" }
  c = transfer(a,1) ! { dg-error "Assumed-rank argument at .1. is only permitted as actual argument to intrinsic inquiry functions" }
  xx = c_loc(a)
end subroutine

subroutine assumed_type (a)
  use iso_c_binding
  type(*), intent(in), target :: a
  integer :: c(1:4)
  type(c_ptr) :: xx
  c = ubound(c,a) ! { dg-error "Assumed-type argument at .1. is only permitted as first actual argument to the intrinsic ubound" }
  c = transfer(a,1) ! { dg-error "Assumed-type argument at .1. is not permitted as actual argument to the intrinsic transfer" }
  xx = c_loc(a)
end subroutine

subroutine no_arg_check (a)
  use iso_c_binding
  integer, intent(in), target :: a
  !gcc$ attributes no_arg_check :: a
  integer :: c(1:4)
  type(c_ptr) :: xx
  c = ubound(c,a) ! { dg-error "Variable with NO_ARG_CHECK attribute at .1. is only permitted as argument to the intrinsic functions C_LOC and PRESENT" }
  c = transfer(a,1) ! { dg-error "Variable with NO_ARG_CHECK attribute at .1. is only permitted as argument to the intrinsic functions C_LOC and PRESENT" }
  xx = c_loc(a)
end subroutine
