/* { dg-options "-std=gnu99" } */
/* { dg-skip-if "test is for emulation" { hard_dfp } { "*" } { "" } } */

/* Check that appropriate exceptions are raised for conversions involving
   decimal float values.  */

#include "fe-check.h"

volatile _Decimal32 d32;
volatile _Decimal64 d64;
volatile _Decimal128 d128;

/* Conversions between decimal float types should raise an invalid
   exception if the value doesn't fit, either because the value
   is too large or the result can't hold the full precision.  */

CONVERT (100, d64, d32, 9.999999e96DD, 0)
CONVERT (101, d64, d32, 10.000000e96DD, FE_INEXACT|FE_OVERFLOW)
CONVERT (102, d64, d32, 1.1111111DD, FE_INEXACT)
CONVERT (110, d128, d32, 9.999999e96DL, 0)
CONVERT (111, d128, d32, 10.000000e96DL, FE_INEXACT|FE_OVERFLOW)
CONVERT (112, d128, d32, 1.1111111DL, FE_INEXACT)
CONVERT (120, d128, d64, 9.999999999999999E384DL, 0)
CONVERT (121, d128, d64, 10.00000000000000E384DL, FE_INEXACT|FE_OVERFLOW)
CONVERT (122, d128, d64, 1.1111111111111111DL, FE_INEXACT)

int
main ()
{
  convert_100 ();
  convert_101 ();
  convert_102 ();
  convert_110 ();
  convert_111 ();
  convert_112 ();
  convert_120 ();
  convert_121 ();
  convert_122 ();

  if (failcnt != 0)
    abort ();
  return 0;
}
