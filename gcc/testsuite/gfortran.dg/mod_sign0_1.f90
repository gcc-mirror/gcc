! { dg-do run }
! PR fortran/49010 
! MOD/MODULO sign of zero.

! We wish to provide the following guarantees:

! MOD(A, P): The result has the sign of A and a magnitude less than
! that of P.  

! MODULO(A, P): The result has the sign of P and a magnitude less than
! that of P.

! Here we test only with constant arguments (evaluated with
! mpfr_fmod), as we don't want to cause failures on targets with a
! crappy libm. But, a target where fmod follows C99 Annex F is
! fine. Also, targets where GCC inline expands fmod (such as x86(-64))
! are also fine.
program mod_sign0_1
  implicit none
  real :: r, t

  r = mod (4., 2.)
  t = sign (1., r)
  if (t < 0.) STOP 1

  r = modulo (4., 2.)
  t = sign (1., r)
  if (t < 0.) STOP 2

  r = mod (-4., 2.)
  t = sign (1., r)
  if (t > 0.) STOP 3

  r = modulo (-4., 2.)
  t = sign (1., r)
  if (t < 0.) STOP 4

  r = mod (4., -2.)
  t = sign (1., r)
  if (t < 0.) STOP 5

  r = modulo (4., -2.)
  t = sign (1., r)
  if (t > 0.) STOP 6

  r = mod (-4., -2.)
  t = sign (1., r)
  if (t > 0.) STOP 7

  r = modulo (-4., -2.)
  t = sign (1., r)
  if (t > 0.) STOP 8

end program mod_sign0_1
