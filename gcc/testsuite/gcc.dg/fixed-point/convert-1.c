/* { dg-do run } */
/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.3 Conversions.

   Check conversions involving fixed-point.  */

extern void abort (void);

#include "convert.h"

int main ()
{
  ALL_CONV (short _Fract, hr);
  ALL_CONV (_Fract, r);
  ALL_CONV (long _Fract, lr);
  ALL_CONV (long long _Fract, llr);

  return 0;
}
