/* { dg-do run } */
/* { dg-options "-O3 -mpower8-vector -Wno-psabi" } */
/* { dg-require-effective-target p8vector_hw } */

#ifndef CHECK_H
#define CHECK_H "sse2-check.h"
#endif

#include CHECK_H

#ifndef TEST
#define TEST sse2_test_pslld_1
#endif

#include <emmintrin.h>

#define TEST_FUNC(id, N) \
  static __m128i \
  __attribute__((noinline, unused)) \
  test##id (__m128i s1) \
  { \
    return _mm_slli_epi32 (s1, N);  \
  }

TEST_FUNC(0, 0)
TEST_FUNC(15, 15)
TEST_FUNC(16, 16)
TEST_FUNC(31, 31)
TEST_FUNC(neg1, -1)
TEST_FUNC(neg16, -16)
TEST_FUNC(neg32, -32)
TEST_FUNC(neg64, -64)
TEST_FUNC(neg128, -128)

#define TEST_CODE(id, N) \
  { \
    int e[4] = {0}; \
    union128i_d u, s; \
    int i; \
    s.x = _mm_set_epi32 (1, -2, 3, 4); \
    u.x = test##id (s.x); \
    if (N >= 0 && N < 32) \
      for (i = 0; i < 4; i++) \
        e[i] = s.a[i] << (N * (N >= 0)); \
    if (check_union128i_d (u, e)) \
      abort (); \
  }

static void
TEST (void)
{
  TEST_CODE(0, 0);
  TEST_CODE(15, 15);
  TEST_CODE(16, 16);
  TEST_CODE(31, 31);
  TEST_CODE(neg1, -1);
  TEST_CODE(neg16, -16);
  TEST_CODE(neg32, -32);
  TEST_CODE(neg64, -64);
  TEST_CODE(neg128, -128);
}
