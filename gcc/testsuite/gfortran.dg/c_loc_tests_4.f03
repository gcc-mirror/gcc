! { dg-do compile }
! { dg-options "-std=f2008" }
!
module c_loc_tests_4
  use, intrinsic :: iso_c_binding
  implicit none

contains
  subroutine sub0() bind(c)
    integer(c_int), target, dimension(10) :: my_array
    integer(c_int), pointer, dimension(:) :: my_array_ptr
    type(c_ptr) :: my_c_ptr

    my_array_ptr => my_array
    my_c_ptr = c_loc(my_array_ptr) ! { dg-error "Noninteroperable array at .1. as argument to C_LOC: Only explicit-size and assumed-size arrays are interoperable" }
  end subroutine sub0
end module c_loc_tests_4
