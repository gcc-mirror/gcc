/* Test floating-point conversions.  _Float32 type with TImode.  */
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
  TEST_I_F(TItype, UTItype, _Float32, FLT32_MANT_DIG, FLT32_MAX_EXP);
  exit (0);
}
