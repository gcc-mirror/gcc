/* { dg-do run } */
/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.3 Conversions.

   Check conversions involving fixed-point.  */

extern void abort (void);

#include "convert.h"

int main ()
{
  ALL_CONV (unsigned short _Accum, uhk);
  ALL_CONV (unsigned _Accum, uk);
  ALL_CONV (unsigned long _Accum, ulk);
  ALL_CONV (unsigned long long _Accum, ullk);

  return 0;
}
