/* { dg-options "-O0" } */

/* Decimal float versions of __builtin_signbit.  */

#include "dfp-dbg.h"

#define CHECK32(D,I) \
  if ((__builtin_signbitd32 (D) != 0) != I) FAILURE

#define CHECK64(D,I) \
  if ((__builtin_signbitd64 (D) != 0) != I) FAILURE

#define CHECK128(D,I) \
  if ((__builtin_signbitd128 (D) != 0) != I) FAILURE

/* Prevent the compiler from folding the calls at compile time.  */
volatile _Decimal32 sd;
volatile _Decimal64 dd;
volatile _Decimal128 td;

int
main ()
{
  sd = 1.9df;  CHECK32 (sd, 0)
  sd = -5.3df; CHECK32 (sd, 1)
  sd = 0.0df;  CHECK32 (sd, 0)
  sd = -0.0df; CHECK32 (sd, 1)

  dd = 1.9dd;  CHECK64 (dd, 0)
  dd = -5.3dd; CHECK64 (dd, 1)
  dd = 0.0dd;  CHECK64 (dd, 0)
  dd = -0.0dd; CHECK64 (dd, 1)

  td = 1.9dl;  CHECK128 (td, 0)
  td = -5.3dl; CHECK128 (td, 1)
  td = 0.0dl;  CHECK128 (td, 0)
  td = -0.0dl; CHECK128 (td, 1)

  FINISH
}
