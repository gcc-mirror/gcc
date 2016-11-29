/* Test floating-point conversions.  Standard types and _Float128.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float128 } */
/* { dg-require-effective-target float128_runtime } */

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>
#include "fp-int-convert.h"

int
main (void)
{
  TEST_I_F(signed char, unsigned char, _Float128, FLT128_MANT_DIG, FLT128_MAX_EXP);
  TEST_I_F(signed short, unsigned short, _Float128, FLT128_MANT_DIG, FLT128_MAX_EXP);
  TEST_I_F(signed int, unsigned int, _Float128, FLT128_MANT_DIG, FLT128_MAX_EXP);
  TEST_I_F(signed long, unsigned long, _Float128, FLT128_MANT_DIG, FLT128_MAX_EXP);
  TEST_I_F(signed long long, unsigned long long, _Float128, FLT128_MANT_DIG, FLT128_MAX_EXP);
  exit (0);
}
