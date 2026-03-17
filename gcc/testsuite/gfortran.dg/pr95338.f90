! { dg-do run }
! { dg-additional-options "-O1 -ff2c" }
!
! PR fortran/95338 - mixed ENTRY results with -ff2c must use the ABI
! return type in the master union.

module m
contains
  function f(x)
    integer :: x
    integer :: f
    real :: g

    f = x
    return

    entry g(x)
    g = x
  end
end

program p
  use m

  if (f(1) /= 1) stop 1
  if (g(1) /= 1.0) stop 2
end
