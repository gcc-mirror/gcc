! { dg-do compile }
!
! Check whether ERF/ERFC take scalars and arrays as arguments (PR31760).
!
PROGRAM test_erf
  REAL :: r = 0.0, ra(2) = (/ 0.0, 1.0 /)

  r  = erf(r)
  r  = erfc(r)

  ra = erf(ra)
  ra = erfc(ra)
END PROGRAM