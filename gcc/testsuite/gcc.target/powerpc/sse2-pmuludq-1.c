/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_pmuludq_1
#endif

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1, __m128i s2)
{
  __asm("" : "+v"(s1), "+v"(s2));
  return _mm_mul_epu32 (s1, s2); 
}

static void
TEST (void)
{
  union128i_d s1, s2;
  union128i_q u;
  long long e[2];
   
  s1.x = _mm_set_epi32 (10,2067,3033,905);
  s2.x = _mm_set_epi32 (11, 9834, 7444, 10222);
  __asm("" : "+v"(s1.x), "+v"(s2.x));
  u.x = test (s1.x, s2.x); 
   
  e[0] = s1.a[0] * s2.a[0];
  e[1] = s1.a[2] * s2.a[2];

  if (check_union128i_q (u, e))
    {
#if DEBUG
      printf ("sse2_test_pmuludq_1; check_union128i_q failed\n");
      printf ("\t ([%x,%x,%x,%x], [%x,%x,%x,%x], -> [%llx, %llx])\n", s1.a[0],
	      s1.a[1], s1.a[2], s1.a[3], s2.a[0], s2.a[1], s2.a[2], s2.a[3],
	      u.a[0], u.a[1]);
      printf ("\t expect [%llx, %llx]\n", e[0], e[1]);
#endif
      abort ();
    }
}
