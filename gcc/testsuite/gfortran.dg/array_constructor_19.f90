! Simplification of unary and binary expressions containing
! array constructors.
!
! See PR33288
!
! { dg-do run }
  real, parameter :: x(1) = 42
  real, parameter :: x1(1) = (/ x /) + 1
  real, parameter :: x2(1) = 1 + (/ x /)
  real, parameter :: x3(1) = -(/ x /)
  real, parameter :: x4(2) = (/ x, 1. /) + (/ 2, (/3/) /)

  if (any (x1 /= (/43./))) STOP 1
  if (any (x2 /= (/43./))) STOP 2
  if (any (x3 /= (/-42./))) STOP 3
  if (any (x4 /= (/44., 4./))) STOP 4
end
