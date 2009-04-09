/* { dg-do run } */
/* { dg-options "-std=gnu99" } */

#include <stdarg.h>
extern void abort (void);

void
f (int a, ...)
{
  va_list ap;
  if (a != 0)
    abort ();
  va_start (ap, a);
  if (va_arg (ap, _Decimal128) != 1.2DL)
    abort ();
  if (va_arg (ap, _Decimal128) != 2.34DL)
    abort ();
  if (va_arg (ap, _Decimal128) != 3.456DL)
    abort ();
  if (va_arg (ap, _Decimal128) != 4.567DL)
    abort ();
  if (va_arg (ap, double) != 5.125)
    abort ();
  va_end (ap);
}

int
main (void)
{
  f (0, 1.2DL, 2.34DL, 3.456DL, 4.567DL, 5.125);
  return 0;
}
