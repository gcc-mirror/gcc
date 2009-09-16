/* Test for bug where fold narrowed decimal floating-point
   operations.  */

#include "dfp-dbg.h"

volatile _Decimal32 f = 1.23456DF;
volatile _Decimal64 d = 1.23456DD;

int
main (void)
{
  if ((_Decimal128)((_Decimal64)f * (_Decimal64)f) != (_Decimal128)(d * d))
    FAILURE
  FINISH
}
