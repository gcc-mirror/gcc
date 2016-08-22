/* Test floating-point conversions.  _Float32x type with TImode.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float32x } */
/* { dg-require-effective-target float32x_runtime } */

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>
#include "fp-int-convert.h"

int
main (void)
{
  TEST_I_F(TItype, UTItype, _Float32x, FLT32X_MANT_DIG, FLT32X_MAX_EXP);
  exit (0);
}
