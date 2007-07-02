! { dg-do compile }
module binding_label_tests_6
  use, intrinsic :: iso_c_binding
  integer(c_int), bind(c, name='my_int') :: my_f90_int_1 ! { dg-error "collides" }
  integer(c_int), bind(c, name='my_int') :: my_f90_int_2 ! { dg-error "collides" }
end module binding_label_tests_6
