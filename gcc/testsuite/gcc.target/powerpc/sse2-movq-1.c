/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_movq_1
#endif

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i b)
{
  __asm("" : "+v"(b));
  return _mm_move_epi64 (b); 
}

static void
TEST (void)
{
  union128i_q u, s1;
  long long e[2] = { 0 };

  s1.x = _mm_set_epi64x(12876, 3376590);
  u.x = test (s1.x);
  e[0] = s1.a[0];

  if (check_union128i_q (u, e))
    {
#if DEBUG
      printf ("sse2_test_movq_1; check_union128i_q failed\n");
      printf ("\t move_epi64 ([%llx, %llx]) -> [%llx, %llx]\n", s1.a[0],
	      s1.a[1], u.a[0], u.a[1]);
      printf ("\t expect [%llx, %llx]\n", e[0], e[1]);
#endif
      abort ();
    }
}
