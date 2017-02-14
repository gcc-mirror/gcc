/* Test floating-point conversions.  Standard types and _Float32.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32 } */
/* { dg-require-effective-target float32_runtime } */

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>
#include "fp-int-convert.h"

int
main (void)
{
  TEST_I_F(signed char, unsigned char, _Float32, FLT32_MANT_DIG, FLT32_MAX_EXP);
  TEST_I_F(signed short, unsigned short, _Float32, FLT32_MANT_DIG, FLT32_MAX_EXP);
  TEST_I_F(signed int, unsigned int, _Float32, FLT32_MANT_DIG, FLT32_MAX_EXP);
  TEST_I_F(signed long, unsigned long, _Float32, FLT32_MANT_DIG, FLT32_MAX_EXP);
  TEST_I_F(signed long long, unsigned long long, _Float32, FLT32_MANT_DIG, FLT32_MAX_EXP);
  exit (0);
}
