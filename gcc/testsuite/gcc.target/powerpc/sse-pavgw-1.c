/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector" } */
/* { dg-require-effective-target p8vector_hw } */

#define NO_WARN_X86_INTRINSICS 1

#ifndef CHECK_H
#define CHECK_H "sse-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse_test_pavgw_1
#endif

#include <xmmintrin.h>

static __m64
__attribute__((noinline, unused))
test (__m64 s1, __m64 s2)
{
  return _mm_avg_pu16 (s1, s2);
}

static void 
TEST (void)
{
  __m64_union u, s1, s2;
  __m64_union e;
  int i, tmp;

  s1.as_m64 = _mm_set_pi16 (1, 2, 3, 4);
  s2.as_m64 = _mm_set_pi16 (11, 98, 76, -100);
  u.as_m64 = test (s1.as_m64, s2.as_m64);

  for (i = 0; i < 4; i++)
    {
      tmp = (unsigned short) s1.as_short[i] + (unsigned short) s2.as_short[i]
	  + 1;
      e.as_short[i] = tmp >> 1;
    }

  if (u.as_m64 != e.as_m64)
    {
#if DEBUG
      printf ("test_mmx_pavgw_1; failed\n");
      printf ("\t _mm_avg_pu16 (%llx, %llx) -> %llx != %llx\n", s1.as_m64,
	      s2.as_m64, u.as_m64, e.as_m64);
#endif
      abort ();
    }
}
