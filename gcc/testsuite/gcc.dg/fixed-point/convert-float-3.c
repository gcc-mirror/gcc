/* { dg-do run } */
/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.3 Conversions.

   Check conversions involving fixed-point.  */

extern void abort (void);

#include "convert.h"

int main ()
{
  ALL_CONV_FLOAT (short _Accum, hk);
  ALL_CONV_FLOAT (_Accum, k);
  ALL_CONV_FLOAT (long _Accum, lk);
  ALL_CONV_FLOAT (long long _Accum, llk);

  return 0;
}
