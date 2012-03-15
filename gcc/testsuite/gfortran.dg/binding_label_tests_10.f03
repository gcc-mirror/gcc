! { dg-do compile }
! This file must be compiled BEFORE binding_label_tests_10_main.f03, which it 
! should be because dejagnu will sort the files.
module binding_label_tests_10
  use iso_c_binding
  implicit none
  integer(c_int), bind(c,name="c_one") :: one
end module binding_label_tests_10
! { dg-final { keep-modules "" } }
