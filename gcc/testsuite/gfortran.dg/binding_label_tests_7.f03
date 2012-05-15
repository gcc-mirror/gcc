! { dg-do compile }
module A
  use, intrinsic :: iso_c_binding, only: c_int
  integer(c_int), bind(c, name='my_c_print') :: my_int ! { dg-error "collides" }
end module A

program main
use A
interface
   subroutine my_c_print() bind(c) ! { dg-error "collides" }
   end subroutine my_c_print
end interface

call my_c_print()
end program main
