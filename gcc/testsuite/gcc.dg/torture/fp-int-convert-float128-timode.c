/* Test floating-point conversions.  __float128 type with TImode.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run { target i?86-*-* x86_64-*-* ia64-*-* } } */
/* { dg-options "" } */

#include "fp-int-convert.h"

#define FLOAT128_MANT_DIG 113

int
main (void)
{
  TEST_I_F(TItype, UTItype, __float128, FLOAT128_MANT_DIG);
  exit (0);
}
