/* Test floating-point conversions.  _Float128x type with TImode.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float128x } */
/* { dg-require-effective-target float128x_runtime } */

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>
#include "fp-int-convert.h"

int
main (void)
{
  TEST_I_F(TItype, UTItype, _Float128x, FLT128X_MANT_DIG, FLT128X_MAX_EXP);
  exit (0);
}
