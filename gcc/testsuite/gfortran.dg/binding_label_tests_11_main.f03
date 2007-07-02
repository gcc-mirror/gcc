! { dg-do compile }
! This file must be compiled AFTER binding_label_tests_11.f03, which it 
! should be because dejagnu will sort the files.
module binding_label_tests_11_main
  use iso_c_binding, only: c_int
  implicit none
contains
  function one() bind(c, name="c_one") ! { dg-error "collides" }
    integer(c_int) one
    one = 1
  end function one
end module binding_label_tests_11_main

program main
  use binding_label_tests_11 ! { dg-error "collides" }
  use binding_label_tests_11_main
end program main

! { dg-final { cleanup-modules "binding_label_tests_11_main binding_label_tests_11" } }
