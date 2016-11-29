/* Test floating-point conversions.  _Float128 type with TImode.  */
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
  TEST_I_F(TItype, UTItype, _Float128, FLT128_MANT_DIG, FLT128_MAX_EXP);
  exit (0);
}
