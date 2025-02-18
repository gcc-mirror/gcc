! Test translation of OMP clause adjust_args
! { dg-do compile }

module main
  use iso_c_binding, only: c_ptr
  implicit none
  type(c_ptr) :: b ! { dg-error "List item 'b' at .1., declared at .2., is not a dummy argument" }
  
contains
  subroutine base2 (a)
    type(c_ptr), intent(inout) :: a
    !$omp declare variant (variant2) match (construct={parallel}) adjust_args (need_device_ptr: a) ! { dg-error "the 'adjust_args' clause can only be specified if the 'dispatch' selector of the construct selector set appears in the 'match' clause at .1." }
  end subroutine
  subroutine base3 (a)
    type(c_ptr), intent(inout) :: a
    !$omp declare variant (variant2) match (construct={dispatch}) adjust_args (need_device_ptr: a) adjust_args (need_device_ptr: a) ! { dg-error "'a' at .1. is specified more than once" }
  end subroutine
  subroutine base4 (a)
    type(c_ptr), intent(inout) :: a
    !$omp declare variant (variant2) match (construct={dispatch}) adjust_args (need_device_ptr: b) ! { dg-error "List item 'b' at .1., declared at .2., is not a dummy argument" }
  end subroutine

  subroutine variant2 (a)
    type(c_ptr), intent(inout) :: a
  end subroutine

end module
