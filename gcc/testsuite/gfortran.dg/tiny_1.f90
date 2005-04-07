! { dg-do run }
! Test program inspired by bug report from Walt Brainerd.
! http://gcc.gnu.org/ml/fortran/2005-04/msg00132.html
program tiny1
  real(4) x4
  real(8) x8
  if (minexponent(x4) /= exponent(tiny(x4))) call abort
  if (minexponent(x8) /= exponent(tiny(x8))) call abort
end program tiny1
