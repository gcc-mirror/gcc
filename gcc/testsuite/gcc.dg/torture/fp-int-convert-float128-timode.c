/* Test floating-point conversions.  __float128 type with TImode.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run } */
/* { dg-require-effective-target __float128 } */
/* { dg-require-effective-target base_quadfloat_support } */
/* { dg-options "" } */
/* { dg-add-options __float128 } */

#include "fp-int-convert.h"

#define FLOAT128_MANT_DIG 113
#define FLOAT128_MAX_EXP 16384

int
main (void)
{
  TEST_I_F(TItype, UTItype, __float128, FLOAT128_MANT_DIG, FLOAT128_MAX_EXP);
  exit (0);
}
