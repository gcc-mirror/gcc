! { dg-do compile }
! This file must be compiled BEFORE binding_label_tests_11_main.f03, which it 
! should be because dejagnu will sort the files.
module binding_label_tests_11
  use iso_c_binding, only: c_int
  implicit none
contains
  function one() bind(c, name="c_one")
    integer(c_int) one
    one = 1
  end function one
end module binding_label_tests_11

! Do not use dg-final to cleanup-modules
