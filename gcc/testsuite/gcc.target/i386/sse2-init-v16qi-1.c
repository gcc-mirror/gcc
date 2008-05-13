/* { dg-do run } */
/* { dg-options "-O2 -msse2" } */

#include "sse2-check.h"

#ifdef DEBUG
#include <stdio.h>
#endif

#include <emmintrin.h>

static void
__attribute__((noinline))
check (__m128i x, unsigned char *v, int j)
{
  union
    {
      __m128i x;
      unsigned char i[16];
    } u;
  unsigned int i;

  u.x = x;
  
  for (i = 0; i < sizeof (v) / sizeof (v[0]); i++)
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
test (unsigned char *v)
{
  __m128i x;

  x = _mm_set_epi8 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, v[0]);
  check (x, v, 0);
  x = _mm_set_epi8 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, v[1], 0);
  check (x, v, 1);
  x = _mm_set_epi8 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, v[2], 0, 0);
  check (x, v, 2);
  x = _mm_set_epi8 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, v[3], 0, 0, 0);
  check (x, v, 3);
  x = _mm_set_epi8 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, v[4], 0, 0, 0, 0);
  check (x, v, 4);
  x = _mm_set_epi8 (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, v[5], 0, 0, 0, 0, 0);
  check (x, v, 5);
  x = _mm_set_epi8 (0, 0, 0, 0, 0, 0, 0, 0, 0, v[6], 0, 0, 0, 0, 0, 0);
  check (x, v, 6);
  x = _mm_set_epi8 (0, 0, 0, 0, 0, 0, 0, 0, v[7], 0, 0, 0, 0, 0, 0, 0);
  check (x, v, 7);
}

static void
sse2_test (void)
{
  unsigned char v[16]
    = { 0x7B, 0x5B, 0x54, 0x65, 0x73, 0x74, 0x56, 0x65,
	0x63, 0x74, 0x6F, 0x72, 0x5D, 0x53, 0x47, 0x5D };
  test (v);
}
