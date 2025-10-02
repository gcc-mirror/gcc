! { dg-do run }
!
! Test the fix for PR121948, in which the PDT constructor expressions without
! the type specification list, ie. relying on default values, failed. The fix
! also required that the incorrect initialization of functions with implicit
! function result be eliminated.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
  implicit none

  integer, parameter :: dp = kind(1d0)
  real, parameter :: ap = 42.0
  real(dp), parameter :: ap_d = 42.0d0

  type operands_t(k)
    integer, kind :: k = kind(1.)
    real(k) :: actual, expected 
  end type

  type(operands_t) :: x
  type(operands_t(dp)) :: y

  x = operands (ap, 10 * ap)
  if (abs (x%actual - ap) >1e-5) stop 1
  if (abs (x%expected - 10 * ap) > 1e-5) stop 2


  y = operands_dp (ap_d, 10d0 * ap_d)
  if (abs (y%actual - ap_d) > 1d-10) stop 3
  if (abs (y%expected - 10d0 * ap_d) > 1d-10) stop 4
  if (kind (y%actual) /= dp) stop 5
  if (kind (y%expected) /= dp) stop 6

contains

  function operands(actual, expected)                    ! Use the default 'k'
    real actual, expected
    type(operands_t) :: operands
    operands = operands_t(actual, expected)
  end function


  function operands_dp(actual, expected)                 ! Override the default
    real(dp) actual, expected
    type(operands_t(dp)) :: operands_dp
    operands_dp = operands_t(dp)(actual, expected) 
  end function

end
