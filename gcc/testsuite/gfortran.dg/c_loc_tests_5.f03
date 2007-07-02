! { dg-do compile }
module c_loc_tests_5
  use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_loc, c_int

contains
  subroutine sub0() bind(c)
    type(c_ptr) :: f_ptr, my_c_ptr
    character(kind=c_char, len=20), target :: format
    integer(c_int), dimension(:), pointer :: int_ptr
    integer(c_int), dimension(10), target :: int_array

    f_ptr = c_loc(format(1:1))

    int_ptr => int_array
    my_c_ptr = c_loc(int_ptr(0))

  end subroutine sub0
end module c_loc_tests_5
! { dg-final { cleanup-modules "c_loc_tests_5" } }
