! { dg-do compile }
program matmul_bounds_1
  implicit none
  real, dimension(3,2) :: a
  real, dimension(2,3) :: b
  real, dimension(3,2) :: rab
  real, dimension(2,2) :: rok
  real, dimension(2) :: rv
  real, dimension(3) :: rw
  real, dimension(3) :: x
  real, dimension(2) :: y
  a = 1
  b = 2
  x = 3
  y = 4
  ! These tests should throw an error
  rab = matmul(a,b) ! { dg-error "Different shape" }
  rv = matmul(a,y) ! { dg-error "Different shape" }
  rv = matmul(x,b) ! { dg-error "Different shape" }
  ! These are ok.
  rw = matmul(a,y)
  rv = matmul(x,a)
  rok = matmul(b,a)
end program matmul_bounds_1

