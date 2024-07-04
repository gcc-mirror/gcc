/* PR c/102989 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <stdarg.h>
#include <stdlib.h>

#if __BITINT_MAXWIDTH__ >= 128
unsigned _BitInt(128) b, c;
#endif
#if __BITINT_MAXWIDTH__ >= 575
signed _BitInt(575) d, e;
#endif

void
foo (int a, ...)
{
  va_list ap;
  va_start (ap, a);
  if (a == 1)
    {
      if (va_arg (ap, _BitInt(2)) != 1wb)
	abort ();
      if (va_arg (ap, _BitInt(3)) != 3wb)
	abort ();
      if (va_arg (ap, _BitInt(15)) != 16383wb)
	abort ();
      if (va_arg (ap, unsigned _BitInt(32)) != 4294967295uwb)
	abort ();
      if (va_arg (ap, _BitInt(64)) != 0x7fffffffffffffffwb)
	abort ();
    }
#if __BITINT_MAXWIDTH__ >= 128
  b = va_arg (ap, unsigned _BitInt(128));
#endif
#if __BITINT_MAXWIDTH__ >= 575
  d = va_arg (ap, _BitInt(575));
#endif
  if (va_arg (ap, int) != 42)
    abort ();
  va_end (ap);
}

void
bar (void)
{
  foo (1, 1wb, 3wb, 16383wb, 4294967295uwb, 9223372036854775807wb,
#if __BITINT_MAXWIDTH__ >= 128
       c,
#endif
#if __BITINT_MAXWIDTH__ >= 575
       e,
#endif
       42);
  foo (2,
#if __BITINT_MAXWIDTH__ >= 128
       c,
#endif
#if __BITINT_MAXWIDTH__ >= 575
       e,
#endif
       42);
}
