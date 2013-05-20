! { dg-do compile }
module binding_label_tests_6
  use, intrinsic :: iso_c_binding
  integer(c_int), bind(c, name='my_int') :: my_f90_int_1 ! { dg-error "Variable my_f90_int_2 from module binding_label_tests_6 with binding label my_int at .1. uses the same global identifier as entity at .2. from module binding_label_tests_6" }
  integer(c_int), bind(c, name='my_int') :: my_f90_int_2 ! { dg-error "Variable my_f90_int_2 from module binding_label_tests_6 with binding label my_int at .1. uses the same global identifier as entity at .2. from module binding_label_tests_6" }
end module binding_label_tests_6
