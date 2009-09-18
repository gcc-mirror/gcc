/* Test for bug where fold wrongly removed conversions to double and
   replaced them by conversions to float.  */

#include "dfp-dbg.h"

volatile float f = __builtin_inff ();
volatile _Decimal32 d32 = 1e40DF;

int
main (void)
{
  if ((double) f == (double) d32)
    FAILURE

  FINISH
}
