/* Test for spurious underflow from __float128 division.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-require-effective-target __float128 } */
/* { dg-require-effective-target base_quadfloat_support } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-add-options __float128 } */

#include <fenv.h>
#include <stdlib.h>

int
main (void)
{
  volatile __float128 a = 0x0.fffp-16382q, b = 0x0.fffp0q, c;
  c = a / b;
  if (fetestexcept (FE_UNDERFLOW | FE_INEXACT))
    abort ();
  if (c != 0x1p-16382q)
    abort ();
  exit (0);
}
