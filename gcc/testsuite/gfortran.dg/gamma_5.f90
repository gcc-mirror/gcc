! { dg-do run }
! PR 33683 - we used to pick up the wrong gamma function
! from the library on some systems.
program main
  implicit none
  integer, parameter :: n_max = 20
  double precision, dimension(0:n_max) :: c
  double precision :: pi
  integer :: n
  double precision :: td, xd
  real :: ts,xs

  pi = 4 * atan(1.d0)
  c(0) = 1.
  do n=1, n_max
     c(n) = (2*n-1)*c(n-1)*0.5d0
  end do

  do n=1, n_max
     xs = n + 0.5
     xd = n + 0.5d0
     td = c(n)*sqrt(pi)
     ts = c(n)*sqrt(pi)
     if (abs(gamma(xs)-ts)/ts > 3e-6) call abort
     if (abs(gamma(xd)-td)/td > 5e-14) call abort
  end do
  call tst_s(2.3, gamma(2.3))
  call tst_s(3.7, gamma(3.7))
  call tst_s(5.5, gamma(5.5))
  call tst_d(4.2d0, gamma(4.2d0))
  call tst_d(8.1d0, gamma(8.1d0))
contains
  subroutine tst_s(a, b)
    real :: a, b
    if (abs(gamma(a) - b)/b > 1e-6) call abort
  end subroutine tst_s

  subroutine tst_d(a, b)
    double precision :: a,b
    if (abs(gamma(a) - b)/b > 5e-14) call abort
  end subroutine tst_d
end program main
