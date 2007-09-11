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
volatile long double tf;

CONVERT_VALID (101, td, sf, 0.000488281251dl, 0.00048828125f, 0.f)

/* 2**(-25) = 0.298023223876953125E-7.  */
CONVERT_VALID (102, td, sf, 2.98023223876953125e-8dl, 2.9802322387695312e-08f,
               01.e-13f)

/* Fractional part doesn't fit.  */
CONVERT_VALID (103, df, sd, 1.0e-20, 1.0e-20df, 0.df)

/* Exact power of 2.  */
CONVERT_VALID (104, df, sd, 0.00048828125, 0.00048828125df, 0.df)
CONVERT_VALID (105, df, sd, 1.0e-96, 0.dd, DEC32_MIN)

/* A value slightly less than FLT_MAX can be converted in both directions.  */
CONVERT_VALID (201, sf, sd, 3.402819e+38f, 3.402819e+38df, 0.df)
CONVERT_VALID (202, sd, sf, 3.402819e+38df, 3.402819e+38f, 0.f)
CONVERT_VALID (203, sf, dd, 3.402819e+38f, 3.402819e+38dd, 1.e+30dd)
CONVERT_VALID (204, dd, sf, 3.402819e+38dd, 3.402819e+38f, 0.f)
CONVERT_VALID (205, sf, td, 3.402819e+38f, 3.402819e+38dl, 1.e+30dl)
CONVERT_VALID (206, td, sf, 3.402819e+38dl, 3.402819e+38f, 0.f)

/* A value slightly less than DEC32_MAX can be converted in both directions.  */
CONVERT_VALID (211, sd, df, 9.999998e96df, 9.999998e96, 0.)
CONVERT_VALID (212, df, sd, 9.999998e96, 9.999998e96df, 0.df)

/* A value slightly less than DBL_MAX can be converted in both directions.  */
CONVERT_VALID (221, df, dd, 1.79768e+308, 1.79768e+308dd, 0.dd)
CONVERT_VALID (222, dd, df, 1.79768e+308dd, 1.79768e+308, 0.)
CONVERT_VALID (223, df, td, 1.79768e+308, 1.79768e+308dl, 1.e292dl)
CONVERT_VALID (224, td, df, 1.79768e+308dl, 1.79768e+308, 0.)

/* An integral value with 6 digits (FLT_DIG) can be converted between float
   and _Decimal32 in both directions.  */
CONVERT_VALID (301, sd, sf, 100000.DF, 100000.F, 0.F)
CONVERT_VALID (302, sf, sd, 100000.F, 100000.DF, 0.DF)
CONVERT_VALID (303, sd, sf, 999999.DF, 999999.F, 0.F)
CONVERT_VALID (304, sf, sd, 999999.F, 999999.DF, 0.DF)

/* An integral value with 7 digits (DEC32_MANT_DIG) can be converted between
   _Decimal32 and double in both directions.  */
CONVERT_VALID (311, sd, df, 1000000.DF, 1000000., 0.)
CONVERT_VALID (312, df, sd, 1000000., 1000000.DF, 0.DF)
CONVERT_VALID (313, sd, df, 9999999.DF, 9999999., 0.)
CONVERT_VALID (314, df, sd, 9999999., 9999999.DF, 0.DF)

/* An integral value with 15 digits (DBL_DIG) can be converted between
   double and _Decimal64 in both directions.  */
CONVERT_VALID (321, dd, df, 100000000000000.DD, 100000000000000., 0.)
CONVERT_VALID (322, df, dd, 100000000000000., 100000000000000.DD, 0.DD);
CONVERT_VALID (323, dd, df, 999999999999999.DD, 999999999999999., 0.);
CONVERT_VALID (324, df, dd, 999999999999999., 999999999999999.DD, 0.DD);

/* If LDBL_DIG is at least 16, an integral value with 16 digits can be
   converted between _Decimal64 and long double in both directions.  */
CONVERT_VALID (331, dd, tf, 1000000000000000.DD, 1000000000000000.L, 0.L)
CONVERT_VALID (332, td, dd, 1000000000000000.L, 1000000000000000.DD, 0.DD)
CONVERT_VALID (333, dd, tf, 9999999999999999.DD, 9999999999999999.L, 0.L)
CONVERT_VALID (334, td, dd, 9999999999999999.L, 9999999999999999.DD, 0.DD)

/* If LDBL_DIG is at least 18, an integral value with 18 digits can be
   converted between long double and _Decimal128 in both directions.  */
CONVERT_VALID (341, td, tf, 100000000000000000.DL, 100000000000000000.L, 0.L)
CONVERT_VALID (342, tf, td, 100000000000000000.L, 100000000000000000.DL, 0.DL)
CONVERT_VALID (343, td, tf, 999999999999999999.DL, 999999999999999999.L, 0.L)
CONVERT_VALID (344, tf, td, 999999999999999999.L, 999999999999999999.DL, 0.DL)

/* If LDBL_DIG is at least 31, an integral value with 31 digits can be
   converted between long double and _Decimal128 in both directions.  */
CONVERT_VALID (351, td, tf, 1000000000000000000000000000000.DL,
			    1000000000000000000000000000000.L, 0.L)
CONVERT_VALID (352, tf, td, 1000000000000000000000000000000.L,
			    1000000000000000000000000000000.DL, 0.DL)
CONVERT_VALID (353, td, tf, 9999999999999999999999999999999.DL,
			    9999999999999999999999999999999.L, 0.L)
CONVERT_VALID (354, tf, td, 9999999999999999999999999999999.L,
			    9999999999999999999999999999999.DL, 0.DL)

/* If LDBL_DIG is at least 33, an integral value with 33 digits can be
   converted between long double and _Decimal128 in both directions.  */
CONVERT_VALID (361, td, tf, 100000000000000000000000000000000.DL,
			    100000000000000000000000000000000.L, 0.L)
CONVERT_VALID (362, tf, td, 100000000000000000000000000000000.L,
			    100000000000000000000000000000000.DL, 0.DL)
CONVERT_VALID (363, td, tf, 999999999999999999999999999999999.DL,
			    999999999999999999999999999999999.L, 0.L)
CONVERT_VALID (364, tf, td, 999999999999999999999999999999999.L,
			    999999999999999999999999999999999.DL, 0.DL)

int
main ()
{
  convert_101 ();
  convert_102 ();
  convert_103 ();
  convert_104 ();
  convert_105 ();

  convert_201 ();
  convert_202 ();
  convert_203 ();
  convert_204 ();
  convert_205 ();
  convert_206 ();

  convert_211 ();
  convert_212 ();

  convert_221 ();
  convert_222 ();
  convert_223 ();
  convert_224 ();

  convert_301 ();
  convert_302 ();
  convert_303 ();
  convert_304 ();

  convert_311 ();
  convert_312 ();
  convert_313 ();
  convert_314 ();

  convert_321 ();
  convert_322 ();
  convert_323 ();
  convert_324 ();

  if (LDBL_DIG >= 16)
    {
      convert_331 ();
      convert_332 ();
      convert_333 ();
      convert_334 ();
    }

  if (LDBL_DIG >= 18)
    {
      convert_341 ();
      convert_342 ();
      convert_343 ();
      convert_344 ();
    }

  if (LDBL_DIG >= 31)
    {
      convert_351 ();
      convert_352 ();
      convert_353 ();
      convert_354 ();
    }

  if (LDBL_DIG >= 33)
    {
      convert_361 ();
      convert_362 ();
      convert_363 ();
      convert_364 ();
    }

  if (failcnt != 0)
    abort ();
  return 0;
}
