/* Test floating-point conversions.  TImode types.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run { xfail lp64 } } */
/* { dg-options "" } */

#include <float.h>
#include "fp-int-convert.h"

int
main (void)
{
  TEST_I_F(TItype, UTItype, float, FLT_MANT_DIG);
  TEST_I_F(TItype, UTItype, double, DBL_MANT_DIG);
  TEST_I_F(TItype, UTItype, long double, LDBL_MANT_DIG);
  exit (0);
}
