! Test resolution of OMP clause adjust_args
! { dg-do compile } 

module main
  use iso_c_binding, only: c_ptr, c_funptr
  implicit none
  interface
    subroutine f1 (i)
      integer, intent(inout) :: i
    end subroutine
    subroutine h (a)
      import c_funptr
      type(c_funptr), intent(inout) :: a
    end subroutine
  end interface
contains

  subroutine f9 (i)  ! { dg-error "Argument 'i' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" }
    integer, intent(inout) :: i
    !$omp declare variant (f1) match (construct={dispatch}) adjust_args (need_device_ptr: i)  ! { dg-error "Argument 'i' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" }
  end subroutine
  subroutine f13 (a)  ! { dg-error "Argument 'a' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" }
    type(c_funptr), intent(inout) :: a
    !$omp declare variant (h) match (construct={dispatch}) adjust_args (need_device_ptr: a)  ! { dg-error "Argument 'a' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" }
  end subroutine

  subroutine test
    integer :: i
    type(c_funptr) :: a
    !$omp dispatch
    call f9(i)
    !$omp dispatch
    call f13(a)
  end subroutine
  
end module
