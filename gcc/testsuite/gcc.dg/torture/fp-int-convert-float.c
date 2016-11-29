/* Test floating-point conversions.  Standard types and float.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run } */
/* { dg-options "" } */

#include <float.h>
#include "fp-int-convert.h"

int
main (void)
{
  TEST_I_F(signed char, unsigned char, float, FLT_MANT_DIG, FLT_MAX_EXP);
  TEST_I_F(signed short, unsigned short, float, FLT_MANT_DIG, FLT_MAX_EXP);
  TEST_I_F(signed int, unsigned int, float, FLT_MANT_DIG, FLT_MAX_EXP);
  TEST_I_F(signed long, unsigned long, float, FLT_MANT_DIG, FLT_MAX_EXP);
  TEST_I_F(signed long long, unsigned long long, float, FLT_MANT_DIG, FLT_MAX_EXP);
  exit (0);
}
