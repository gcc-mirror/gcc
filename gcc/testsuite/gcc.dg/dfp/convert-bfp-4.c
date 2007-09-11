/* { dg-options "-std=gnu99" } */

/* This test assumes IEEE float and double.  It also tests long double
   but makes no assumption about its size or range of values.  */

#include "convert.h"

volatile _Decimal32 sd;
volatile _Decimal64 dd;
volatile _Decimal128 td;
volatile float sf;
volatile double df;
volatile long double tf;   /* might actually be df or xf, doesn't matter */

CONVERT_INF_ALL (t);

int
main ()
{
  CALL_INF_ALL (t)

  if (failcnt != 0)
    abort ();
  return 0;
}
