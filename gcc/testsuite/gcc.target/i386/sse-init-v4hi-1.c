/* { dg-do run } */
/* { dg-options "-O2 -msse" } */
/* { dg-require-effective-target sse } */

#include "sse-check.h"

#ifdef DEBUG
#include <stdio.h>
#endif

#include <mmintrin.h>

static void
__attribute__((noinline))
check (__m64 x, unsigned short *v, int j)
{
  union
    {
      __m64 x;
      unsigned short i[8];
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
test (unsigned short *v)
{
  __m64 x;

  x = _mm_set_pi16 (0, 0, 0, v[0]);
  check (x, v, 0);
  x = _mm_set_pi16 (0, 0, v[1], 0);
  check (x, v, 1);
  x = _mm_set_pi16 (0, v[2], 0, 0);
  check (x, v, 2);
  x = _mm_set_pi16 (v[3], 0, 0, 0);
  check (x, v, 3);
}

static void
sse_test (void)
{
  unsigned short v[4]
    = { 0x7B5B, 0x5465, 0x7374, 0x5665};
  test (v);
}
