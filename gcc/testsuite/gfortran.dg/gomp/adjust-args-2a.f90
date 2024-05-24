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

  subroutine f9 (i)
    integer, intent(inout) :: i
    !$omp declare variant (f1) match (construct={dispatch}) adjust_args (need_device_ptr: i) ! { dg-error "argument list item 'i' in 'need_device_ptr' at .1. must be of TYPE.C_PTR." }
  end subroutine
  subroutine f13 (a)
    type(c_funptr), intent(inout) :: a
    !$omp declare variant (h) match (construct={dispatch}) adjust_args (need_device_ptr: a) ! { dg-error "argument list item 'a' in 'need_device_ptr' at .1. must be of TYPE.C_PTR." }
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
