/* { dg-options "-std=gnu99" } */

/* Check that appropriate exceptions are raised for BFP to DFP conversions.
   The test only uses double and _Decimal32; tests for conversions to
   _Decimal64 would need 128-bit long double.  */

#include "fe-check.h"

volatile _Decimal32 d32;
volatile double d;

CONVERT (100, d, d32, 1.0e96, FE_INEXACT)
CONVERT (101, d, d32, 1.0e97, FE_OVERFLOW|FE_INEXACT) 
CONVERT (102, d, d32, -1.0e96, FE_INEXACT)
CONVERT (103, d, d32, -1.0e97, FE_OVERFLOW|FE_INEXACT) 

#ifdef __DECIMAL_BID_FORMAT__
/* These only result in fp exceptions with BID. DPD doesn't work.  */
CONVERT (104, d, d32, 1.0e-96, FE_UNDERFLOW|FE_INEXACT)
CONVERT (105, d, d32, 0.00048828125, FE_INEXACT)  /* exact power of 2 */
#endif

int
main ()
{
  convert_100 ();
  convert_101 ();
  convert_102 ();
  convert_103 ();
#ifdef __DECIMAL_BID_FORMAT__
  convert_104 ();
  convert_105 ();
#endif

  if (failcnt != 0)
    abort ();
  return 0;
}
