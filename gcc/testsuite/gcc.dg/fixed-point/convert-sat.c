/* { dg-do run } */
/* { dg-options "-std=gnu99 -O0" } */

/* C99 6.3 Conversions.

   Check conversions involving fixed-point.  */

extern void abort (void);

#include "convert.h"

int main ()
{
  SAT_CONV1 (short _Accum, hk);
  SAT_CONV1 (_Accum, k);
  SAT_CONV1 (long _Accum, lk);
  SAT_CONV1 (long long _Accum, llk);

  SAT_CONV2 (unsigned short _Accum, uhk);
  SAT_CONV2 (unsigned _Accum, uk);
  SAT_CONV2 (unsigned long _Accum, ulk);
  SAT_CONV2 (unsigned long long _Accum, ullk);

  SAT_CONV3 (short _Fract, hr);
  SAT_CONV3 (_Fract, r);
  SAT_CONV3 (long _Fract, lr);
  SAT_CONV3 (long long _Fract, llr);

  SAT_CONV4 (signed char);
  SAT_CONV4 (short);
  SAT_CONV4 (int);
  SAT_CONV4 (long);
  SAT_CONV4 (long long);

  SAT_CONV5 (unsigned char);
  SAT_CONV5 (unsigned short);
  SAT_CONV5 (unsigned int);
  SAT_CONV5 (unsigned long);
  SAT_CONV5 (unsigned long long);

  SAT_CONV6 (float);
  SAT_CONV6 (double);

  return 0;
}
