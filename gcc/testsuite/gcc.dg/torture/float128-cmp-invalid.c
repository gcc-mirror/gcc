/* Test for "invalid" exceptions from __float128 comparisons.  */
/* { dg-do run { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>
#include <stdlib.h>

int
main (void)
{
  volatile __float128 a = __builtin_nanq (""), b = 0.0q;
  volatile int r = a < b;
  if (!fetestexcept (FE_INVALID))
    abort ();
  if (r)
    abort ();
  exit (0);
}
