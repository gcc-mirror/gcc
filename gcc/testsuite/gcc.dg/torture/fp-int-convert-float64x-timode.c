/* Test floating-point conversions.  _Float64x type with TImode.  */
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
  TEST_I_F(TItype, UTItype, _Float64x, FLT64X_MANT_DIG, FLT64X_MAX_EXP);
  exit (0);
}
