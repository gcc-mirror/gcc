/* Test extensions to __float128 quiet signaling NaNs.  */
/* { dg-do run } */
/* { dg-options "-fsignaling-nans" } */
/* { dg-require-effective-target __float128 } */
/* { dg-require-effective-target base_quadfloat_support } */
/* { dg-require-effective-target fenv_exceptions } */
/* { dg-add-options __float128 } */

#include <fenv.h>
#include <float.h>
#include <stdlib.h>

volatile long double a = __builtin_nansl ("");

int
main (void)
{
#if LDBL_MANT_DIG < 113
  volatile __float128 r = a;
  feclearexcept (FE_INVALID);
  r += 1;
  if (fetestexcept (FE_INVALID))
    abort ();
#endif
  exit (0);
}
