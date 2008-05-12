/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

#include "sse2-check.h"

#ifdef DEBUG
#include <stdio.h>
#endif

#include <emmintrin.h>

static void
__attribute__((noinline))
test (unsigned int *v)
{
  union
    {
      __m128i x;
      unsigned int i[4];
    } u;
  unsigned int i;
  
  u.x = _mm_set_epi32 (v[3], v[2], v[1], v[0]);

  for (i = 0; i < sizeof (v) / sizeof (v[0]); i++)
    if (v[i] != u.i[i])
      {
#ifdef DEBUG
	printf ("%i: 0x%x != 0x%x\n", i, v[i], u.i[i]);
#endif
	abort ();
      }
}

static void
sse2_test (void)
{
  unsigned int v[4]
    = { 0x7B5B5465, 0x73745665, 0x63746F72, 0x5D53475D };
  test (v);
}
