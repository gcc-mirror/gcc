/* PR target/128751 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2" } */

#include <stdarg.h>

struct E { unsigned : 8; };

__attribute__((noipa)) long
g (long a, long b, long c, long d, long f, long h, struct E e, ...)
{
  va_list ap;
  (void) e;
  va_start (ap, e);
  long x = va_arg (ap, long);
  va_end (ap);
  return x + a + b + c + d + f + h - 21;
}

int
main ()
{
  struct E e = {};
  if (g (1, 2, 3, 4, 5, 6, e, 42L) != 42)
    __builtin_abort ();
  return 0;
}
