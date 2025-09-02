! { dg-do compile }
! { dg-options "-std=f2018" }
program lower
  use iso_c_binding
  type(c_ptr) :: x
  integer, target :: array_2d(12)
  integer, pointer :: ptr_2d(:, :)
  integer :: myshape_2d(2)
  integer :: mylower_2d(2)

  array_2d = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
  x = c_loc(array_2d)
  myshape_2d = [3, 4]
  mylower_2d = [2, 2]

  call c_f_pointer(x, ptr_2d, shape=myshape_2d, lower=mylower_2d) ! { dg-error "Fortran 2023: LOWER argument at" }
end program lower
