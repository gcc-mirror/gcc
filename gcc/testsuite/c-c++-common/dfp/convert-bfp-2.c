/* This test assumes IEEE float and double.  It also tests long double
   but makes no assumption about its size or range of values.  */

#include "convert.h"

volatile _Decimal32 sd;
volatile _Decimal64 dd;
volatile _Decimal128 td;
volatile float sf;
volatile double df;
volatile long double tf;   /* might actually be df or xf, doesn't matter */

CONVERT_VALID_ALL (t1, 0.0, 0.)
CONVERT_VALID_ALL (t2, 1.0, 0.)
CONVERT_VALID_ALL (t3, -11.5, 0.)
CONVERT_VALID_ALL (t4, 7.0, 0.1e-14)
CONVERT_VALID_ALL (t5, -7.0, 0.1e-14)
CONVERT_VALID_ALL (t6, 999999., 0.)
CONVERT_VALID_ALL (t7, -999999., 0.)

int
main ()
{
  CALL_VALID_ALL (t1)
  CALL_VALID_ALL (t2)
  CALL_VALID_ALL (t3)
  CALL_VALID_ALL (t4)
  CALL_VALID_ALL (t5)
  CALL_VALID_ALL (t6)
  CALL_VALID_ALL (t7)

  FINISH
}
