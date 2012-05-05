! { dg-do run }
! PR fortran/24518 
! MOD/MODULO of large arguments.
! The naive algorithm goes pear-shaped for large arguments, instead
! use fmod.
! Here we test only with constant arguments (evaluated with
! mpfr_fmod), as we don't want to cause failures on targets with a
! crappy libm.
program mod_large_1
  implicit none
  real :: r1
  r1 = mod (1e22, 1.7)
  if (abs(r1 - 0.995928764) > 1e-5) call abort
  r1 = modulo (1e22, -1.7)
  if (abs(r1 + 0.704071283) > 1e-5) call abort
end program mod_large_1
