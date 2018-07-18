/* { dg-do run } */
/* { dg-require-effective-target sse4 } */
/* { dg-options "-O2 -msse4.1" } */

#include "sse4_1-check.h"

#ifdef DEBUG
#include <stdio.h>
#endif

#include <emmintrin.h>

static void
__attribute__((noinline))
check (__m128i x, unsigned int *v, int j)
{
  union
    {
      __m128i x;
      unsigned int i[4];
    } u;
  unsigned int i;

  u.x = x;
  
  for (i = 0; i < sizeof (u) / sizeof (v[0]); i++)
    if (i == j)
      {
	if (v[i] != u.i[i])
	  {
#ifdef DEBUG
	    printf ("%i: 0x%x != 0x%x\n", i, v[i], u.i[i]);
#endif
	    abort ();
	  }
      }
    else if (u.i[i] != 0)
      {
#ifdef DEBUG
	printf ("%i: 0x%x != 0\n", i, u.i[i]);
#endif
	abort ();
      }
}

static void
__attribute__((noinline))
test (unsigned int *v)
{
  __m128i x;

  x = _mm_set_epi32 (0, 0, 0, v[0]);
  check (x, v, 0);
  x = _mm_set_epi32 (0, 0, v[1], 0);
  check (x, v, 1);
  x = _mm_set_epi32 (0, v[2], 0, 0);
  check (x, v, 2);
  x = _mm_set_epi32 (v[3], 0, 0, 0);
  check (x, v, 3);
}

static void
sse4_1_test (void)
{
  unsigned int v[4]
    = { 0x7B5B5465, 0x73745665, 0x63746F72, 0x5D53475D };
  test (v);
}
