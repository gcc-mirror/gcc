/* { dg-do run } */
/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.3 Conversions.

   Check conversions involving fixed-point.  */

extern void abort (void);

#include "convert.h"

int main ()
{
  ALL_ACCUM_CONV (short _Accum, hk);
  ALL_ACCUM_CONV (_Accum, k);
  ALL_ACCUM_CONV (long _Accum, lk);
  ALL_ACCUM_CONV (long long _Accum, llk);
  ALL_ACCUM_CONV (unsigned short _Accum, uhk);
  ALL_ACCUM_CONV (unsigned _Accum, uk);
  ALL_ACCUM_CONV (unsigned long _Accum, ulk);
  ALL_ACCUM_CONV (unsigned long long _Accum, ullk);

  NEG_CONV (short _Fract, hr);
  NEG_CONV (_Fract, r);
  NEG_CONV (long _Fract, lr);
  NEG_CONV (long long _Fract, llr);
  NEG_CONV (short _Accum, hk);
  NEG_CONV (_Accum, k);
  NEG_CONV (long _Accum, lk);
  NEG_CONV (long long _Accum, llk);

  return 0;
}
