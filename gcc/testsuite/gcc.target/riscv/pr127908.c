/* { dg-do run } */
/* { dg-additional-options "-std=c11" } */

#include <fenv.h>
#include <math.h>

#define DEF(NAME, FN, T)				\
  __attribute__((noipa)) void				\
  NAME (const T *s, T *d, long n)			\
  {							\
    for (; n > 0; --n, ++s, ++d)			\
      *d = FN (*s);					\
  }

DEF (ceil_d, ceil, double)
DEF (floor_d, floor, double)
DEF (trunc_d, trunc, double)
DEF (ceil_f, ceilf, float)
DEF (floor_f, floorf, float)
DEF (trunc_f, truncf, float)

#define CHECK(NAME, T, IN)				\
  do							\
    for (int i = 0; i < 4; i++)				\
      {							\
	T out;						\
	feclearexcept (FE_ALL_EXCEPT);			\
	NAME (&(IN)[i], &out, 1);			\
	if (fetestexcept (FE_INVALID))			\
	  __builtin_abort ();				\
      }							\
  while (0)

int
main (void)
{
  const double in_d[] = { INFINITY, -INFINITY, 1e300, 2.5 };
  const float in_f[] = { INFINITY, -INFINITY, 1e30f, 2.5f };

  CHECK (ceil_d, double, in_d);
  CHECK (floor_d, double, in_d);
  CHECK (trunc_d, double, in_d);
  CHECK (ceil_f, float, in_f);
  CHECK (floor_f, float, in_f);
  CHECK (trunc_f, float, in_f);

  __builtin_exit (0);
}
