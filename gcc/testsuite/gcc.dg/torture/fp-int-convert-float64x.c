/* Test floating-point conversions.  Standard types and _Float64x.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float64x } */
/* { dg-require-effective-target float64x_runtime } */

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>
#include "fp-int-convert.h"

int
main (void)
{
  TEST_I_F(signed char, unsigned char, _Float64x, FLT64X_MANT_DIG, FLT64X_MAX_EXP);
  TEST_I_F(signed short, unsigned short, _Float64x, FLT64X_MANT_DIG, FLT64X_MAX_EXP);
  TEST_I_F(signed int, unsigned int, _Float64x, FLT64X_MANT_DIG, FLT64X_MAX_EXP);
  TEST_I_F(signed long, unsigned long, _Float64x, FLT64X_MANT_DIG, FLT64X_MAX_EXP);
  TEST_I_F(signed long long, unsigned long long, _Float64x, FLT64X_MANT_DIG, FLT64X_MAX_EXP);
  exit (0);
}
