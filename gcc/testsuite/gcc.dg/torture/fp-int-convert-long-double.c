/* Test floating-point conversions.  Standard types and long double.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* Skipped for MIPS16 targets because of PR 35232 */
/* { dg-do run { target { { ! mips*-*-* } || nomips16 } } } */
/* { dg-options "" } */

#include <float.h>
#include "fp-int-convert.h"

int
main (void)
{
  TEST_I_F(signed char, unsigned char, long double, LDBL_MANT_DIG);
  TEST_I_F(signed short, unsigned short, long double, LDBL_MANT_DIG);
  TEST_I_F(signed int, unsigned int, long double, LDBL_MANT_DIG);
  TEST_I_F(signed long, unsigned long, long double, LDBL_MANT_DIG);
  TEST_I_F(signed long long, unsigned long long, long double, LDBL_MANT_DIG);
  exit (0);
}
