! { dg-do run }
! PR libfortran/99218 - matmul on temporary array accesses invalid memory

program p
  implicit none
  integer, parameter :: nState = 300000
  integer, parameter :: nCon = 1
  real,    parameter :: ZERO = 0.0
  real :: G(nCon,nState) = ZERO
  real :: H(nState,nCon) = ZERO
  real :: lambda(nCon)   = ZERO
  real :: f(nState)      = ZERO
  f = matmul (transpose (G), lambda)
  if (f(1) /= ZERO) stop 1
end program
