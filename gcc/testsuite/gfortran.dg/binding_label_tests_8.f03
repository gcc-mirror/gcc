! { dg-do compile }
module binding_label_tests_8
  use, intrinsic :: iso_c_binding, only: c_int
  integer(c_int), bind(c, name='my_f90_sub') :: my_c_int ! { dg-error "collides" }

contains
  subroutine my_f90_sub() bind(c) ! { dg-error "collides" }
  end subroutine my_f90_sub
end module binding_label_tests_8
