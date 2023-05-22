/* Test floating-point conversions.  _Float64 type with TImode.  */
/* { dg-do run } */
/* { dg-options "" } */
/* { dg-add-options float64 } */
/* { dg-require-effective-target float64_runtime } */
/* { dg-skip-if "double support is incomplete" { "avr-*-*" } } */

#define __STDC_WANT_IEC_60559_TYPES_EXT__
#include <float.h>
#include "fp-int-convert.h"

int
main (void)
{
  TEST_I_F(TItype, UTItype, _Float64, FLT64_MANT_DIG, FLT64_MAX_EXP);
  exit (0);
}
