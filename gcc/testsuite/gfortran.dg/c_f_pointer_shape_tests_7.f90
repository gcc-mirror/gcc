! { dg-do run }
! { dg-options "-std=f2023" }
program lower
  use iso_c_binding
  type(c_ptr) :: x
  integer, target :: array_2d(12), array_3d(24)
  integer, pointer :: ptr_2d(:, :), ptr_3d(:, :, :)
  integer :: myshape_2d(2), myshape_3d(3)
  integer :: mylower_2d(2), mylower_3d(3)

  array_2d = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
  x = c_loc(array_2d)
  myshape_2d = [3, 4]
  mylower_2d = [2, 2]

  call c_f_pointer(x, ptr_2d, shape=myshape_2d, lower=mylower_2d)
  if (any(lbound(ptr_2d) /= [2, 2])) stop 1
  if (any(ubound(ptr_2d) /= [4, 5])) stop 2
  if (any(shape(ptr_2d) /= [3, 4])) stop 3
  if (ptr_2d(2, 2) /= 1) stop 4
  if (ptr_2d(3, 4) /= 8) stop 5
  if (ptr_2d(4, 5) /= 12) stop 6

  array_3d = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24]
  x = c_loc(array_3d)
  myshape_3d = [2, 3, 4]
  mylower_3d = [-1, -2, -3]

  call c_f_pointer(x, ptr_3d, shape=myshape_3d, lower=mylower_3d)
  if (any(lbound(ptr_3d) /= [-1, -2, -3])) stop 7
  if (any(ubound(ptr_3d) /= [0, 0, 0])) stop 8
  if (any(shape(ptr_3d) /= [2, 3, 4])) stop 9
  if (ptr_3d(0, 0, 0) /= 24) stop 10

end program lower
