! { dg-do compile }
! This file must be compiled BEFORE binding_label_tests_13_main.f03, which it 
! should be because dejagnu will sort the files.  
module binding_label_tests_13
 use, intrinsic :: iso_c_binding, only: c_int
  integer(c_int) :: c3
  bind(c) c3
end module binding_label_tests_13
! { dg-final { keep-modules "" } }
