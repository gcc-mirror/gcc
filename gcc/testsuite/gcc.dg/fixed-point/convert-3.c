/* { dg-do run } */
/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.3 Conversions.

   Check conversions involving fixed-point.  */

extern void abort (void);

#include "convert.h"

int main ()
{
  ALL_CONV (short _Accum, hk);
  ALL_CONV (_Accum, k);
  ALL_CONV (long _Accum, lk);
  ALL_CONV (long long _Accum, llk);

  return 0;
}
