/* { dg-do run { target bitint } } */
/* { dg-additional-options "-std=c23" } */

#include <stdarg.h>

static _BitInt(65)
sum65 (int n, ...)
{
  va_list ap;
  va_start (ap, n);

  _BitInt(65) s = 0;

  while (n--)
    s += va_arg (ap, _BitInt(65));

  va_end (ap);
  return s;
}

static _BitInt(128)
sum128 (int n, ...)
{
  va_list ap;
  va_start (ap, n);

  _BitInt(128) s = 0;

  while (n--)
    s += va_arg (ap, _BitInt(128));

  va_end (ap);
  return s;
}

int
main (void)
{
  _BitInt(65) a = (_BitInt(65)) 1 << 40;
  _BitInt(65) b = (_BitInt(65)) 1 << 39;

  if (sum65 (2, a, b) != a + b)
    __builtin_abort ();

  _BitInt(128) x = (_BitInt(128)) 1 << 100;
  _BitInt(128) y = (_BitInt(128)) 123456789;

  if (sum128 (2, x, y) != x + y)
    __builtin_abort ();

  return 0;
}
