/* { dg-do run } */
/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.3 Conversions.

  Check conversions involving fixed-point.  */

extern void abort (void);

#include "convert.h"

int main ()
{
  ALL_CONV_FLOAT (unsigned short _Fract, uhr);
  ALL_CONV_FLOAT (unsigned _Fract, ur);
  ALL_CONV_FLOAT (unsigned long _Fract, ulr);
  ALL_CONV_FLOAT (unsigned long long _Fract, ullr);

  return 0;
}
