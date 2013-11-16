/* Test extensions to __float128 quiet signaling NaNs.  */
/* { dg-do run { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "-fsignaling-nans" } */
/* { dg-require-effective-target fenv_exceptions } */

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
