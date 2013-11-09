/* Test for spurious underflow from __float128 division.  */
/* { dg-do run { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "" } */

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
