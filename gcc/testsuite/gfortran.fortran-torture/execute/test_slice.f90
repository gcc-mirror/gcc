! Program to test handling of reduced rank array sections.  This uncovered
! bugs in simplify_shape and the scalarization of array sections.
program test_slice
  implicit none
  
  real (kind = 8), dimension(2, 2, 2) :: x
  real (kind = 8) :: min, max

  x = 1.0
  if (minval(x(1, 1:2, 1:1)) .ne. 1.0) STOP 1
  if (maxval(x(1, 1:2, 1:1)) .ne. 1.0) STOP 2
  if (any (shape(x(1, 1:2, 1:1)) .ne. (/2, 1/))) STOP 3

  if (any (shape(x(1, 1:2, 1)) .ne. (/2/))) STOP 4
  if (any (shape(x(1:1, 1:2, 1:1)) .ne. (/1, 2, 1/))) STOP 5

end program test_slice
