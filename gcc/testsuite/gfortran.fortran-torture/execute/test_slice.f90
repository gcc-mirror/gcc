! Program to test handling of reduced rank array sections.  This uncovered
! bugs in simplify_shape and the scalarization of array sections.
program test_slice
  implicit none
  
  real (kind = 8), dimension(2, 2, 2) :: x
  real (kind = 8) :: min, max

  x = 1.0
  if (minval(x(1, 1:2, 1:1)) .ne. 1.0) call abort ()
  if (maxval(x(1, 1:2, 1:1)) .ne. 1.0) call abort ()
  if (any (shape(x(1, 1:2, 1:1)) .ne. (/2, 1/))) call abort ()

  if (any (shape(x(1, 1:2, 1)) .ne. (/2/))) call abort ()
  if (any (shape(x(1:1, 1:2, 1:1)) .ne. (/1, 2, 1/))) call abort ()

end program test_slice
