! { dg-do run }
! { dg-options "-fbounds-check -ffrontend-optimize" }
! { dg-shouldfail "Fortran runtime error: Incorrect extent in argument B in MATMUL intrinsic for dimension 2: is 1, should be 2" }
module x
  implicit none
contains
  subroutine mmul(c, a, b)
    real, dimension(:,:), intent(in) :: a,b
    real, dimension(:,:), intent(out) :: c
    c = matmul(a,transpose(b))
  end subroutine mmul
end module x

program main
  use x
  integer, parameter :: n = 3, m=4, cnt=2
  real, dimension(n,cnt) :: a
  real, dimension(m,cnt-1) :: b
  real, dimension(n,m) :: c
  a = 1.0
  b = 2.3
  call mmul(c,a,b)
end program main
