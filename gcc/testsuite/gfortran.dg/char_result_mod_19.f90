! { dg-do  run }
! { dg-additional-sources char_result_19.f90 }
!
! Module for char_result_19.f90
! Tests fix for PR86248
!
module test_module
  implicit none
  public :: func_1
  private
  character(len=*),dimension(0:2),parameter :: darray = (/"el0 ","el11","el2 "/)
contains
  function func_1 (func_1_input) result(f)
    integer, intent(in) :: func_1_input
    character(len = len_trim (darray(func_1_input))) :: f
    f = darray(func_1_input)
  end function func_1
end module test_module
