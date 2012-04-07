! { dg-do run }
! { dg-options "-ffrontend-optimize" }
! Do not move common functions out of implicit DO loop constructors.
program test
  integer, parameter :: N = 4
  integer, parameter :: dp=kind(1.d0)
  real(kind=dp), parameter :: pi=4*atan(1._dp)
  real(kind=dp), parameter :: eps = 1.e-14_dp
  real(kind=dp) :: h1(0:N-1), h2(0:N-1)
  integer i

  i = 1
  h1 = [(cos(2*pi*mod(i*k,N)/N),k=0,N/2), &
       & (sin(2*pi*mod(i*k,N)/N),k=1,N/2-1)]
  h2 = (/ 1._dp, 0._dp, -1._dp, 1._dp /)
  if (any(abs(h1 - h2) > eps)) call abort
end program test
