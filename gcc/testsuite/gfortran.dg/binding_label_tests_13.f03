! { dg-do compile }
module binding_label_tests_13
 use, intrinsic :: iso_c_binding, only: c_int
  integer(c_int) :: c3
  bind(c) c3
end module binding_label_tests_13
