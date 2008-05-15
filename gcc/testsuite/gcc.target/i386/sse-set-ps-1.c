/* { dg-do run } */
/* { dg-options "-O2 -msse" } */

#include "sse-check.h"

#ifdef DEBUG
#include <stdio.h>
#endif

#include <xmmintrin.h>

static void
__attribute__((noinline))
test (float *v)
{
  union
    {
      __m128 x;
      float f[4];
    } u;
  unsigned int i;
  
  u.x = _mm_set_ps (v[3], v[2], v[1], v[0]);

  for (i = 0; i < sizeof (v) / sizeof (v[0]); i++)
    if (v[i] != u.f[i])
      {
#ifdef DEBUG
	printf ("%i: %f != %f\n", i, v[i], u.f[i]);
#endif
	abort ();
      }
}

static void
sse_test (void)
{
  float v[4] = { -3, 2, 1, 9 };
  test (v);
}
