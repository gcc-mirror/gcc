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

/* Check values that are too large for the result type.  */
CONVERT_TO_PINF (301, sd, sf, 4.e+38df, f)
CONVERT_TO_PINF (303, dd, sf, 4.e+38dd, f)
CONVERT_TO_PINF (302, sd, sf, 9.9e+384df, f)
CONVERT_TO_PINF (304, dd, sf, 9.9e+384dd, f)
CONVERT_TO_PINF (305, td, sf, 4.e+38dl, f)
CONVERT_TO_PINF (306, td, sf, 1.e+39dl, f)
CONVERT_TO_MINF (311, sd, sf, -4.e+38df, f)
CONVERT_TO_MINF (312, dd, sf, -4.e+38dd, f)
CONVERT_TO_MINF (313, sd, sf, -9.9e+384df, f)
CONVERT_TO_MINF (314, dd, sf, -9.9e+384dd, f)
CONVERT_TO_MINF (315, td, sf, -4.e+38dl, f)
CONVERT_TO_MINF (316, td, sf, -1.e+39dl, f)

CONVERT_TO_PINF (321, dd, df, 1.8e+308dd,)
CONVERT_TO_PINF (322, dd, df, 9.9e+384dd,)
CONVERT_TO_PINF (323, td, df, 1.8e+308dl,)
CONVERT_TO_PINF (324, td, df, 9.9e+384dl,)
CONVERT_TO_PINF (325, dd, df, 1.e309dd,)
CONVERT_TO_PINF (326, td, df, 1.e309dl,)
CONVERT_TO_MINF (331, dd, df, -1.8e+308dd,)
CONVERT_TO_MINF (332, dd, df, -9.9e+384dd,)
CONVERT_TO_MINF (333, td, df, -1.8e+308dl,)
CONVERT_TO_MINF (334, td, df, -9.9e+384dl,)
CONVERT_TO_MINF (335, dd, df, -1.e309dd,)
CONVERT_TO_MINF (336, td, df, -1.e309dl,)

CONVERT_TO_PINF (341, df, sd, 1.0e+97, d32)
CONVERT_TO_PINF (342, df, sd, 1.6e+308, d32)
CONVERT_TO_MINF (351, df, sd, -1.0e+97, d32)
CONVERT_TO_MINF (352, df, sd, -1.6e+308, d32)

int
main ()
{
  convert_301 ();
  convert_302 ();
  convert_303 ();
  convert_304 ();
  convert_305 ();
  convert_306 ();
  convert_311 ();
  convert_312 ();
  convert_313 ();
  convert_314 ();
  convert_315 ();
  convert_316 ();

  convert_321 ();
  convert_322 ();
  convert_323 ();
  convert_324 ();
  convert_325 ();
  convert_326 ();
  convert_331 ();
  convert_332 ();
  convert_333 ();
  convert_334 ();
  convert_335 ();
  convert_336 ();

  convert_341 ();
  convert_342 ();
  convert_351 ();
  convert_352 ();

  if (failcnt != 0)
    abort ();
  return 0;
}
