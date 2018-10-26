/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_pshufd_1
#endif

#define N 0xec

#include <emmintrin.h>

static __m128i
__attribute__((noinline, unused))
test (__m128i s1)
{
  return _mm_shuffle_epi32 (s1, N); 
}

static void
TEST (void)
{
  union128i_d u, s1;
  int e[4] = { 0 };
  int i;
   
  s1.x = _mm_set_epi32 (16,15,14,13);
  u.x = test (s1.x);

  for (i = 0; i < 4; i++)
    e[i] = s1.a[((N & (0x3<<(2*i)))>>(2*i))];

  if (check_union128i_d(u, e))
    {
#if DEBUG
      printf ("sse2_test_pshufd_1; check_union128i_d failed\n");
      printf ("\t ([%x,%x,%x,%x]) -> [%x,%x,%x,%x]\n", s1.a[0], s1.a[1],
	      s1.a[2], s1.a[3], u.a[0], u.a[1], u.a[2], u.a[3]);
      printf ("\t expect [%x,%x,%x,%x]\n", e[0], e[1], e[2], e[3]);
#endif
      abort ();
    }
}
