/* Test truncation from __float128 to float uses after-rounding
   tininess detection.  */

/* { dg-do run { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>
#include <stdlib.h>

int
main (void)
{
  volatile __float128 a = 0x0.ffffffffffp-126q, b = 0x0.ffffffp-126q;
  volatile float r;
  r = (float) a;
  if (fetestexcept (FE_UNDERFLOW))
    abort ();
  if (r != 0x1p-126f)
    abort ();
  r = (float) b;
  if (!fetestexcept (FE_UNDERFLOW))
    abort ();
  if (r != 0x1p-126f)
    abort ();
  exit (0);
}
