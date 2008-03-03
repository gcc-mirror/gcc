! { dg-do compile }
!
! Check whether BESXY functions take scalars and
! arrays as arguments (PR31760).
!
PROGRAM test_erf
  REAL :: r = 0.0, ra(2) = (/ 0.0, 1.0 /)

  r  = BESJ0(r)
  r  = BESJ1(r)
  r  = BESJN(0, r)

  r  = BESY0(r)
  r  = BESY1(r)
  r  = BESYN(0, r)

  ra = BESJ0(ra)
  ra = BESJ1(ra)
  ra = BESJN(0, ra)

  ra = BESY0(ra)
  ra = BESY1(ra)
  ra = BESYN(0, ra)

  r  = BESSEL_J0(r)
  r  = BESSEL_J1(r)
  r  = BESSEL_JN(0, r)

  r  = BESSEL_Y0(r)
  r  = BESSEL_Y1(r)
  r  = BESSEL_YN(0, r)

  ra = BESSEL_J0(ra)
  ra = BESSEL_J1(ra)
  ra = BESSEL_JN(0, ra)

  ra = BESSEL_Y0(ra)
  ra = BESSEL_Y1(ra)
  ra = BESSEL_YN(0, ra)

END PROGRAM
