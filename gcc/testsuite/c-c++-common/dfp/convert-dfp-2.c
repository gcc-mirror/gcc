/* { dg-options "-O0" } */

/* Test decimal fp conversions of zero.  */

#include "dfp-dbg.h"

volatile _Decimal32 d32a, d32c;
volatile _Decimal64 d64a, d64c;
volatile _Decimal128 d128a, d128c;

int
main ()
{
  d32a = d32c;
  if (d32a)
    FAILURE
  d32a = d64c;
  if (d32a)
    FAILURE
  d32a = d128c;
  if (d32a)
    FAILURE

  d64a = d32c;
  if (d64a)
    FAILURE
  d64a = d64c;
  if (d64a)
    FAILURE
  d64a = d128c;
  if (d64a)
    FAILURE
  
  d128a = d32c;
  if (d128a)
    FAILURE
  d128a = d64c;
  if (d128a)
    FAILURE
  d128a = d128c;
  if (d128a)
    FAILURE
  
  FINISH
}
