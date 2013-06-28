! { dg-do run }
! { dg-options "-fbounds-check -fno-realloc-lhs" }
! { dg-shouldfail "Incorrect extent in return value of TRANSPOSE intrinsic in dimension 1: is 2, should be 3" }
program main
  implicit none
  character(len=10) :: in
  real, dimension(:,:), allocatable :: a,b
  integer :: ax, ay, bx, by

  in = "2 2 3 2"
  read (unit=in,fmt='(4I2)') ax, ay, bx, by
  allocate (a(ax,ay))
  allocate (b(bx,by))
  a = 1.0
  b = 2.1
  b = transpose(a)
end program main
! { dg-output "Fortran runtime error: Array bound mismatch for dimension 1 of array 'b' \\(3/2\\)" }
