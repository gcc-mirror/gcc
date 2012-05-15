! { dg-do run }
! { dg-additional-sources c_ptr_tests_7_driver.c }
module c_ptr_tests_7
  use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr

contains
  function func0() bind(c)
    type(c_ptr) :: func0
    func0 = c_null_ptr
  end function func0
end module c_ptr_tests_7
