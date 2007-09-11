/* { dg-options "-std=gnu99 -w" } */

/* This test assumes IEEE float and double.  */

#define __STDC_WANT_DEC_FP__
#include <float.h>

#include "convert.h"

volatile _Decimal32 sd;
volatile _Decimal64 dd;
volatile _Decimal128 td;
volatile float sf;
volatile double df;

/* Values slightly smaller than minimum (closest to zero) for result type.  */
CONVERT_VALID (401, sd, sf, 1.e-39df, 0.f, FLT_MIN)
CONVERT_VALID (402, sd, sf, -1.e-39df, 0.f, FLT_MIN)
CONVERT_VALID (403, sd, sf, 1.1e-38df, 0.f, FLT_MIN)
CONVERT_VALID (404, sd, sf, -1.1e-38df, 0.f, FLT_MIN)

CONVERT_VALID (411, dd, sf, 1.e-39dd, 0.f, FLT_MIN)
CONVERT_VALID (412, dd, sf, -1.e-39dd, 0.f, FLT_MIN)
CONVERT_VALID (413, dd, sf, 1.1e-38dd, 0.f, FLT_MIN)
CONVERT_VALID (414, dd, sf, -1.1e-38dd, 0.f, FLT_MIN)

CONVERT_VALID (421, dd, df, 3.e-309dd, 0., DBL_MIN)
CONVERT_VALID (422, dd, df, -3.e-309dd, 0., DBL_MIN)
CONVERT_VALID (423, dd, df, 2.e-308dd, 0., DBL_MIN)
CONVERT_VALID (424, dd, df, -2.e-308dd, 0., DBL_MIN)

CONVERT_VALID (431, td, sf, 1.e-39dl, 0.f, FLT_MIN)
CONVERT_VALID (432, td, sf, -1.e-39dl, 0.f, FLT_MIN)
CONVERT_VALID (433, td, sf, 1.1e-38dl, 0.f, FLT_MIN)
CONVERT_VALID (434, td, sf, -1.1e-38dl, 0.f, FLT_MIN)

CONVERT_VALID (441, td, df, 3.e-309dl, 0., DBL_MIN)
CONVERT_VALID (442, td, df, -3.e-309dl, 0., DBL_MIN)
CONVERT_VALID (443, td, df, 2.e-308dl, 0., DBL_MIN)
CONVERT_VALID (444, td, df, -2.e-308dl, 0., DBL_MIN)

int
main ()
{
  convert_401 ();
  convert_402 ();
  convert_403 ();
  convert_404 ();

  convert_411 ();
  convert_412 ();
  convert_413 ();
  convert_414 ();

  convert_421 ();
  convert_422 ();
  convert_423 ();
  convert_424 ();

  convert_431 ();
  convert_432 ();
  convert_433 ();
  convert_434 ();

  convert_441 ();
  convert_442 ();
  convert_443 ();
  convert_444 ();

  if (failcnt != 0)
    abort ();
  return 0;
}
