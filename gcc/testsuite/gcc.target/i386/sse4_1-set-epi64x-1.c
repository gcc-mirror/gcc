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
test (unsigned long long *v)
{
  union
    {
      __m128i x;
      unsigned long long i[2];
    } u;
  unsigned int i;
  
  u.x = _mm_set_epi64x (v[1], v[0]);

  for (i = 0; i < sizeof (u) / sizeof (v[0]); i++)
    if (v[i] != u.i[i])
      {
#ifdef DEBUG
	printf ("%i: 0x%llx != 0x%llx\n", i, v[i], u.i[i]);
#endif
	abort ();
      }
}

static void
sse4_1_test (void)
{
  unsigned long long v[2]
    = { 0x7B5B546573745665LL, 0x63746F725D53475DLL };
  test (v);
}
