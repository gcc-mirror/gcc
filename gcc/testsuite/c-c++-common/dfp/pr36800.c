/* { dg-do run } */

#include <stdarg.h>
#include "dfp-dbg.h"

void
f (int a, ...)
{
  va_list ap;
  if (a != 0)
    FAILURE
  va_start (ap, a);
  if (va_arg (ap, _Decimal128) != 1.2DL)
    FAILURE
  if (va_arg (ap, _Decimal128) != 2.34DL)
    FAILURE
  if (va_arg (ap, _Decimal128) != 3.456DL)
    FAILURE
  if (va_arg (ap, _Decimal128) != 4.567DL)
    FAILURE
  if (va_arg (ap, double) != 5.125)
    FAILURE
  va_end (ap);
}

int
main (void)
{
  f (0, 1.2DL, 2.34DL, 3.456DL, 4.567DL, 5.125);

  FINISH
}
