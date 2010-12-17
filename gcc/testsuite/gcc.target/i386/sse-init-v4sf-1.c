/* { dg-do run } */
/* { dg-options "-O2 -msse" } */
/* { dg-require-effective-target sse } */

#include "sse-check.h"

#ifdef DEBUG
#include <stdio.h>
#endif

#include <xmmintrin.h>

static void
__attribute__((noinline))
check (__m128 x, float *v, int j)
{
  union
    {
      __m128 x;
      float f[4];
    } u;
  unsigned int i;

  u.x = x;
  
  for (i = 0; i < sizeof (v) / sizeof (v[0]); i++)
    if (i == j)
      {
	if (v[i] != u.f[i])
	  {
#ifdef DEBUG
	    printf ("%i: %f != %f\n", i, v[i], u.f[i]);
#endif
	    abort ();
	  }
      }
    else if (u.f[i] != 0)
      {
#ifdef DEBUG
	printf ("%i: %f != 0\n", i, u.f[i]);
#endif
	abort ();
      }
}

static void
__attribute__((noinline))
test (float *v)
{
  __m128 x;

  x = _mm_set_ps (0, 0, 0, v[0]);
  check (x, v, 0);
  x = _mm_set_ps (0, 0, v[1], 0);
  check (x, v, 1);
  x = _mm_set_ps (0, v[2], 0, 0);
  check (x, v, 2);
  x = _mm_set_ps (v[3], 0, 0, 0);
  check (x, v, 3);
}

static void
sse_test (void)
{
  float v[4] = { -3, 2, 1, 9 };
  test (v);
}
