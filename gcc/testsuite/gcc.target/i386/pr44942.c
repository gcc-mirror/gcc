/* PR target/44942 */
/* { dg-do run { target { ! ia32 } } } */

#include <stdarg.h>
#include <emmintrin.h>

void
test1 (double a, double b, double c, double d, double e, double f,
       double g, __m128d h, ...)
{
  double i;
  va_list ap;

  va_start (ap, h);
  i = va_arg (ap, double);
  if (i != 1234.0)
    __builtin_abort ();
  va_end (ap);
}

void
test2 (double a, double b, double c, double d, double e, double f, double g,
       __m128d h, double i, __m128d j, double k, __m128d l,
       double m, __m128d n, ...)
{
  double o;
  va_list ap;

  va_start (ap, n);
  o = va_arg (ap, double);
  if (o != 1234.0)
    __builtin_abort ();
  va_end (ap);
}

int
main ()
{
  __m128d m = _mm_set1_pd (7.0);
  test1 (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, m, 1234.0);
  test2 (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, m, 0.0, m,
	 0.0, m, 0.0, m, 1234.0);
  return 0;
}
