/* Test truncation from __float128 to double uses after-rounding
   tininess detection.  */

/* { dg-do run { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>
#include <stdlib.h>

int
main (void)
{
  volatile __float128 a = 0x0.fffffffffffffffp-1022q, b = 0x0.fffffffffffff8p-1022q;
  volatile double r;
  r = (double) a;
  if (fetestexcept (FE_UNDERFLOW))
    abort ();
  if (r != 0x1p-1022)
    abort ();
  r = (double) b;
  if (!fetestexcept (FE_UNDERFLOW))
    abort ();
  if (r != 0x1p-1022)
    abort ();
  exit (0);
}
