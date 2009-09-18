/* Test for bug where fold changed binary operation to decimal
   depending on typedefs.  */

#include "dfp-dbg.h"

volatile double d = 1.2345675;

typedef const volatile _Decimal32 d32;

int
main (void)
{
  _Decimal32 a = (d * d);
  d32 b = (d * d);
  if (a != b)
    FAILURE
  FINISH
}
