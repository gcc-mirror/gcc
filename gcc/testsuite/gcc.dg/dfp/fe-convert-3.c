/* { dg-options "-std=gnu99" } */

/* Check that appropriate exceptions are raised for int to DFP conversions.  */

#include "fe-check.h"

volatile _Decimal32 d32;
volatile _Decimal64 d64;
volatile signed int si;
volatile unsigned int ui;
volatile signed long long sll;
volatile unsigned long long ull;

CONVERT (100, si, d32, 9999999, 0)
CONVERT (101, si, d32, 11111111, FE_INEXACT)
CONVERT (102, si, d32, -9999999, 0)
CONVERT (103, si, d32, -10000001, FE_INEXACT)
CONVERT (110, ui, d32, 9999999, 0)
CONVERT (111, ui, d32, 10000001, FE_INEXACT)
CONVERT (200, sll, d64, 9999999999999999, 0)
CONVERT (201, sll, d64, 10000000000000001, FE_INEXACT)
CONVERT (202, sll, d64, -9999999999999999, 0)
CONVERT (203, sll, d64, -10000000000000001, FE_INEXACT)
CONVERT (210, ull, d64, 9999999999999999, 0)
CONVERT (211, ull, d64, 10000000000000001, FE_INEXACT)

int
main ()
{
  if (sizeof (long long) != 16)
    return 0;

  convert_100 ();
  convert_101 ();
  convert_102 ();
  convert_103 ();
  convert_110 ();
  convert_111 ();
  convert_200 ();
  convert_201 ();
  convert_202 ();
  convert_203 ();
  convert_210 ();
  convert_211 ();

  if (failcnt != 0)
    abort ();
  return 0;
}
