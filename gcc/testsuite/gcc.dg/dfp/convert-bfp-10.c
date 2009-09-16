/* This test assumes IEEE float and double.  */

#define __STDC_WANT_DEC_FP__
#include <float.h>

#include "convert.h"

volatile _Decimal32 sd;
volatile _Decimal64 dd;
volatile _Decimal128 td;
volatile float sf;
volatile double df;

/* Conversions using denormalized float values.  */
CONVERT_VALID (111, sf, sd, 1.2e-38f, 1.2e-38df, 0.df)
CONVERT_VALID (112, sf, sd, 1.1e-38f, 1.1e-38df, 0.df)
CONVERT_VALID (113, sf, sd, 1.1e-40f, 1.1e-40df, 1.1e-45df)

CONVERT_VALID (121, sd, sf, 1.2e-38df, 1.2e-38f, 0.f)
CONVERT_VALID (122, sd, sf, 1.1e-38df, 1.1e-38f, 0.f)

CONVERT_VALID (131, sf, sd, -1.2e-38f, -1.2e-38df, 0.df)
CONVERT_VALID (132, sf, sd, -1.1e-38f, -1.1e-38df, 0.df)

CONVERT_VALID (141, sd, sf, -1.2e-38df, -1.2e-38f, 0.f)
CONVERT_VALID (142, sd, sf, -1.1e-38df, -1.1e-38f, 0.f)

/* Conversions using denormalized double values.  */
CONVERT_VALID (211, df, sd, 1.2e-38, 1.2e-38df, 0.df)
CONVERT_VALID (212, df, sd, 1.1e-38, 1.1e-38df, 0.df)
CONVERT_VALID (213, df, sd, 1.e-40, 1.e-40df, 0.df)
CONVERT_VALID (214, df, sd, 8.e-44, 8.e-44df, 0.df)
CONVERT_VALID (215, df, sd, 9.e-44, 9.e-44df, 0.df)
CONVERT_VALID (216, df, sd, 8.e-46, 8.e-46df, 0.df)
CONVERT_VALID (217, df, sd, 7.e-46, 7.e-46df, 0.df)

CONVERT_VALID (221, sd, df, 1.2e-38df, 1.2e-38, 1.e-53)
CONVERT_VALID (222, sd, df, 1.1e-38df, 1.1e-38, 1.e-53)
CONVERT_VALID (223, sd, df, 1.e-40df, 1.e-40, 0.)
CONVERT_VALID (224, sd, df, 8.e-44df, 8.e-44, 0.)
CONVERT_VALID (225, sd, df, 9.e-44df, 9.e-44, 0.)
CONVERT_VALID (226, sd, df, 8.e-46df, 8.e-46, 0.)
CONVERT_VALID (227, sd, df, 7.e-46df, 7.e-46, 0.)

CONVERT_VALID (231, df, sd, -1.2e-38, -1.2e-38df, 0.df)
CONVERT_VALID (232, df, sd, -1.1e-38f, -1.1e-38df, 0.df)
CONVERT_VALID (233, df, sd, -1.e-40, -1.e-40df, 0.df)
CONVERT_VALID (234, df, sd, -8.e-44, -8.e-44df, 0.df)
CONVERT_VALID (235, df, sd, -9.e-44, -9.e-44df, 0.df)
CONVERT_VALID (236, df, sd, -8.e-46, -8.e-46df, 0.df)
CONVERT_VALID (237, df, sd, -7.e-46, -7.e-46df, 0.df)

CONVERT_VALID (241, sd, df, -1.2e-38df, -1.2e-38, 1.e-53)
CONVERT_VALID (242, sd, df, -1.1e-38df, -1.1e-38, 1.e-53)
CONVERT_VALID (243, sd, df, -1.e-40df, -1.e-40, 0.)
CONVERT_VALID (244, sd, df, -8.e-44df, -8.e-44, 0.)
CONVERT_VALID (245, sd, df, -9.e-44df, -9.e-44, 0.)
CONVERT_VALID (246, sd, df, -8.e-46df, -8.e-46, 0.)
CONVERT_VALID (247, sd, df, -7.e-46df, -7.e-46, 0.)

int
main ()
{
  convert_111 ();
  convert_112 ();
  convert_113 ();

  convert_121 ();
  convert_122 ();

  convert_131 ();
  convert_132 ();

  convert_141 ();
  convert_142 ();

  convert_211 ();
  convert_212 ();
  convert_213 ();
  convert_214 ();
  convert_215 ();
  convert_216 ();
  convert_217 ();

  convert_221 ();
  convert_222 ();
  convert_223 ();
  convert_224 ();
  convert_225 ();
  convert_226 ();
  convert_227 ();

  convert_231 ();
  convert_232 ();
  convert_233 ();
  convert_234 ();
  convert_235 ();
  convert_236 ();
  convert_237 ();

  convert_241 ();
  convert_242 ();
  convert_243 ();
  convert_244 ();
  convert_245 ();
  convert_246 ();
  convert_247 ();

  FINISH
}
