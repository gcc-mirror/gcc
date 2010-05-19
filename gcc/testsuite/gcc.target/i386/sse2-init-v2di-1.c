/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "sse2-check.h"

#ifdef DEBUG
#include <stdio.h>
#endif

#include <emmintrin.h>

static void
__attribute__((noinline))
check (__m128i x, unsigned long long *v, int j)
{
  union
    {
      __m128i x;
      unsigned long long i[2];
    } u;
  unsigned int i;

  u.x = x;
  
  for (i = 0; i < sizeof (v) / sizeof (v[0]); i++)
    if (i == j)
      {
	if (v[i] != u.i[i])
	  {
#ifdef DEBUG
	    printf ("%i: 0x%llx != 0x%llx\n", i, v[i], u.i[i]);
#endif
	    abort ();
	  }
      }
    else if (u.i[i] != 0)
      {
#ifdef DEBUG
	printf ("%i: 0x%llx != 0\n", i, u.i[i]);
#endif
	abort ();
      }
}

static void
__attribute__((noinline))
test (unsigned long long *v)
{
  __m128i x;

  x = _mm_set_epi64x (0, v[0]);
  check (x, v, 0);
  x = _mm_set_epi64x (v[1], 0);
  check (x, v, 1);
}

static void
sse2_test (void)
{
  unsigned long long v[2]
    = { 0x7B5B546573745665LL, 0x63746F725D53475DLL };
  test (v);
}
