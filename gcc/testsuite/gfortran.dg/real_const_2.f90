! { dg-do run }
!
! Free form test program for PR 17941 (signed constants with spaces)
!
program real_const_2
  complex c0, c1, c2, c3, c4
  real rp(4), rn(4)
  parameter (c0 = (-0.5, -     0.5))
  parameter (c1 = (-     0.5, +     0.5))
  parameter (c2 = (-    0.5E2, +0.5))
  parameter (c3 = (-0.5, +     0.5E-2))
  parameter (c4 = (-     1, +     1))
  data rn /- 1.0, - 1d0, - 1.d0, - 10.d-1/
  data rp /+ 1.0, + 1d0, + 1.d0, + 10.d-1/
  real, parameter :: del = 1.e-5

  if (abs(c0 - cmplx(-0.5,-0.5)) > del) call abort
  if (abs(c1 - cmplx(-0.5,+0.5)) > del) call abort
  if (abs(c2 - cmplx(-0.5E2,+0.5)) > del) call abort
  if (abs(c3 - cmplx(-0.5,+0.5E-2)) > del) call abort
  if (abs(c4 - cmplx(-1.0,+1.0)) > del) call abort
  if (any (abs (rp - 1.0) > del)) call abort
  if (any (abs (rn + 1.0) > del)) call abort
end program
