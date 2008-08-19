/* Test floating-point conversions.  TImode types.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do run { xfail { ia64-*-hpux* && lp64 } } } */
/* { dg-options "" } */

#include <float.h>
#include "fp-int-convert.h"

int
main (void)
{
  TEST_I_F(TItype, UTItype, float, FLT_MANT_DIG);
  TEST_I_F(TItype, UTItype, double, DBL_MANT_DIG);
  /* Disable the long double tests when using IBM Extended Doubles.
     They have variable precision, but constants calculated by gcc's
     real.c assume fixed precision.  */
#if DBL_MANT_DIG != LDBL_MANT_DIG  && LDBL_MANT_DIG != 106
  TEST_I_F(TItype, UTItype, long double, LDBL_MANT_DIG);
#endif
  exit (0);
}
