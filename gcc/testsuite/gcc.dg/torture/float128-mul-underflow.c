/* Test __float128 multiplication uses after-rounding tininess
   detection.  */

/* { dg-do run { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "" } */
/* { dg-require-effective-target fenv_exceptions } */

#include <fenv.h>
#include <stdlib.h>

int
main (void)
{
  volatile __float128 a = 0x1.fffffffffffffffp-16382q, b = 0x1.0000000000000008p-1q, c;
  c = a * b;
  if (fetestexcept (FE_UNDERFLOW))
    abort ();
  if (c != 0x1p-16382q)
    abort ();
  exit (0);
}
