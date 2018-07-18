/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

__attribute__((noinline, noclone)) int
test_reduce_add_epi32 (__m512i a)
{
  return _mm512_reduce_add_epi32 (a);
}

__attribute__((noinline, noclone)) int
test_reduce_mul_epi32 (__m512i a)
{
  return _mm512_reduce_mul_epi32 (a);
}

__attribute__((noinline, noclone)) int
test_reduce_and_epi32 (__m512i a)
{
  return _mm512_reduce_and_epi32 (a);
}

__attribute__((noinline, noclone)) int
test_reduce_or_epi32 (__m512i a)
{
  return _mm512_reduce_or_epi32 (a);
}

__attribute__((noinline, noclone)) int
test_mask_reduce_add_epi32 (__mmask16 u, __m512i a)
{
  return _mm512_mask_reduce_add_epi32 (u, a);
}

__attribute__((noinline, noclone)) int
test_mask_reduce_mul_epi32 (__mmask16 u, __m512i a)
{
  return _mm512_mask_reduce_mul_epi32 (u, a);
}

__attribute__((noinline, noclone)) int
test_mask_reduce_and_epi32 (__mmask16 u, __m512i a)
{
  return _mm512_mask_reduce_and_epi32 (u, a);
}

__attribute__((noinline, noclone)) int
test_mask_reduce_or_epi32 (__mmask16 u, __m512i a)
{
  return _mm512_mask_reduce_or_epi32 (u, a);
}

__attribute__((noinline, noclone)) int
test_reduce_min_epi32 (__m512i a)
{
  return _mm512_reduce_min_epi32 (a);
}

__attribute__((noinline, noclone)) int
test_reduce_max_epi32 (__m512i a)
{
  return _mm512_reduce_max_epi32 (a);
}

__attribute__((noinline, noclone)) unsigned int
test_reduce_min_epu32 (__m512i a)
{
  return _mm512_reduce_min_epu32 (a);
}

__attribute__((noinline, noclone)) unsigned int
test_reduce_max_epu32 (__m512i a)
{
  return _mm512_reduce_max_epu32 (a);
}

__attribute__((noinline, noclone)) int
test_mask_reduce_min_epi32 (__mmask16 u, __m512i a)
{
  return _mm512_mask_reduce_min_epi32 (u, a);
}

__attribute__((noinline, noclone)) int
test_mask_reduce_max_epi32 (__mmask16 u, __m512i a)
{
  return _mm512_mask_reduce_max_epi32 (u, a);
}

__attribute__((noinline, noclone)) unsigned int
test_mask_reduce_min_epu32 (__mmask16 u, __m512i a)
{
  return _mm512_mask_reduce_min_epu32 (u, a);
}

__attribute__((noinline, noclone)) unsigned int
test_mask_reduce_max_epu32 (__mmask16 u, __m512i a)
{
  return _mm512_mask_reduce_max_epu32 (u, a);
}

__attribute__((noinline, noclone)) float
test_reduce_add_ps (__m512 a)
{
  return _mm512_reduce_add_ps (a);
}

__attribute__((noinline, noclone)) float
test_reduce_mul_ps (__m512 a)
{
  return _mm512_reduce_mul_ps (a);
}

__attribute__((noinline, noclone)) float
test_mask_reduce_add_ps (__mmask16 u, __m512 a)
{
  return _mm512_mask_reduce_add_ps (u, a);
}

__attribute__((noinline, noclone)) float
test_mask_reduce_mul_ps (__mmask16 u, __m512 a)
{
  return _mm512_mask_reduce_mul_ps (u, a);
}

__attribute__((noinline, noclone)) float
test_reduce_min_ps (__m512 a)
{
  return _mm512_reduce_min_ps (a);
}

__attribute__((noinline, noclone)) float
test_reduce_max_ps (__m512 a)
{
  return _mm512_reduce_max_ps (a);
}

__attribute__((noinline, noclone)) float
test_mask_reduce_min_ps (__mmask16 u, __m512 a)
{
  return _mm512_mask_reduce_min_ps (u, a);
}

__attribute__((noinline, noclone)) float
test_mask_reduce_max_ps (__mmask16 u, __m512 a)
{
  return _mm512_mask_reduce_max_ps (u, a);
}

__attribute__((noinline, noclone)) long long
test_reduce_add_epi64 (__m512i a)
{
  return _mm512_reduce_add_epi64 (a);
}

__attribute__((noinline, noclone)) long long
test_reduce_mul_epi64 (__m512i a)
{
  return _mm512_reduce_mul_epi64 (a);
}

__attribute__((noinline, noclone)) long long
test_reduce_and_epi64 (__m512i a)
{
  return _mm512_reduce_and_epi64 (a);
}

__attribute__((noinline, noclone)) long long
test_reduce_or_epi64 (__m512i a)
{
  return _mm512_reduce_or_epi64 (a);
}

__attribute__((noinline, noclone)) long long
test_mask_reduce_add_epi64 (__mmask8 u, __m512i a)
{
  return _mm512_mask_reduce_add_epi64 (u, a);
}

__attribute__((noinline, noclone)) long long
test_mask_reduce_mul_epi64 (__mmask8 u, __m512i a)
{
  return _mm512_mask_reduce_mul_epi64 (u, a);
}

__attribute__((noinline, noclone)) long long
test_mask_reduce_and_epi64 (__mmask8 u, __m512i a)
{
  return _mm512_mask_reduce_and_epi64 (u, a);
}

__attribute__((noinline, noclone)) long long
test_mask_reduce_or_epi64 (__mmask8 u, __m512i a)
{
  return _mm512_mask_reduce_or_epi64 (u, a);
}

__attribute__((noinline, noclone)) long long
test_reduce_min_epi64 (__m512i a)
{
  return _mm512_reduce_min_epi64 (a);
}

__attribute__((noinline, noclone)) long long
test_reduce_max_epi64 (__m512i a)
{
  return _mm512_reduce_max_epi64 (a);
}

__attribute__((noinline, noclone)) unsigned long long
test_reduce_min_epu64 (__m512i a)
{
  return _mm512_reduce_min_epu64 (a);
}

__attribute__((noinline, noclone)) unsigned long long
test_reduce_max_epu64 (__m512i a)
{
  return _mm512_reduce_max_epu64 (a);
}

__attribute__((noinline, noclone)) long long
test_mask_reduce_min_epi64 (__mmask8 u, __m512i a)
{
  return _mm512_mask_reduce_min_epi64 (u, a);
}

__attribute__((noinline, noclone)) long long
test_mask_reduce_max_epi64 (__mmask8 u, __m512i a)
{
  return _mm512_mask_reduce_max_epi64 (u, a);
}

__attribute__((noinline, noclone)) unsigned long long
test_mask_reduce_min_epu64 (__mmask8 u, __m512i a)
{
  return _mm512_mask_reduce_min_epu64 (u, a);
}

__attribute__((noinline, noclone)) unsigned long long
test_mask_reduce_max_epu64 (__mmask8 u, __m512i a)
{
  return _mm512_mask_reduce_max_epu64 (u, a);
}

__attribute__((noinline, noclone)) double
test_reduce_add_pd (__m512d a)
{
  return _mm512_reduce_add_pd (a);
}

__attribute__((noinline, noclone)) double
test_reduce_mul_pd (__m512d a)
{
  return _mm512_reduce_mul_pd (a);
}

__attribute__((noinline, noclone)) double
test_mask_reduce_add_pd (__mmask8 u, __m512d a)
{
  return _mm512_mask_reduce_add_pd (u, a);
}

__attribute__((noinline, noclone)) double
test_mask_reduce_mul_pd (__mmask8 u, __m512d a)
{
  return _mm512_mask_reduce_mul_pd (u, a);
}

__attribute__((noinline, noclone)) double
test_reduce_min_pd (__m512d a)
{
  return _mm512_reduce_min_pd (a);
}

__attribute__((noinline, noclone)) double
test_reduce_max_pd (__m512d a)
{
  return _mm512_reduce_max_pd (a);
}

__attribute__((noinline, noclone)) double
test_mask_reduce_min_pd (__mmask8 u, __m512d a)
{
  return _mm512_mask_reduce_min_pd (u, a);
}

__attribute__((noinline, noclone)) double
test_mask_reduce_max_pd (__mmask8 u, __m512d a)
{
  return _mm512_mask_reduce_max_pd (u, a);
}

#define TESTOP(opname, op, type, suffix, neutral) \
  do {									\
    type r1 = _mm512_reduce_##opname##_##suffix (v.x);			\
    type r2 = test_reduce_##opname##_##suffix (v.x);			\
    type r3 = neutral;							\
    if (r1 != r2)							\
      __builtin_abort ();						\
    for (int i = 0; i < SIZE; i++)					\
      r3 = r3 op v.a[i];						\
    if (r1 != r3)							\
      __builtin_abort ();						\
    type r4 = _mm512_mask_reduce_##opname##_##suffix (MASK_VALUE, v.x);	\
    type r5 = test_mask_reduce_##opname##_##suffix (MASK_VALUE, v.x);	\
    if (r4 != r5)							\
      __builtin_abort ();						\
    r3 = neutral;							\
    for (int i = 0; i < SIZE; i++)					\
      if (MASK_VALUE & (1 << i))					\
	r3 = r3 op v.a[i];						\
    if (r4 != r3)							\
      __builtin_abort ();						\
    type r6 = _mm512_mask_reduce_##opname##_##suffix (0, v.x);		\
    type r7 = test_mask_reduce_##opname##_##suffix (0, v.x);		\
    if (r6 != r7 || r6 != neutral)					\
      __builtin_abort ();						\
  } while (0)

#define SIZE (AVX512F_LEN / 32)
#include "avx512f-mask-type.h"

#define TEST_EPI32(c1, c2, c3, c4, c5, c6, c7, c8, \
		   c9, c10, c11, c12, c13, c14, c15, c16)		\
  do {									\
    UNION_TYPE (AVX512F_LEN, i_d) v;					\
    v.x = _mm512_set_epi32 (c1, c2, c3, c4, c5, c6, c7, c8,		\
			    c9, c10, c11, c12, c13, c14, c15, c16);	\
    TESTOP (add, +, int, epi32, 0);					\
    TESTOP (mul, *, int, epi32, 1);					\
    TESTOP (and, &, int, epi32, ~0);					\
    TESTOP (or, |, int, epi32, 0);					\
    TESTOP (min, < v.a[i] ? r3 :, int, epi32, __INT_MAX__);		\
    TESTOP (max, > v.a[i] ? r3 :, int, epi32, -__INT_MAX__ - 1);	\
    TESTOP (min, < (unsigned) v.a[i] ? r3 :, unsigned, epu32, ~0U);	\
    TESTOP (max, > (unsigned) v.a[i] ? r3 :, unsigned, epu32, 0);	\
  } while (0)

#define TEST_PS(c1, c2, c3, c4, c5, c6, c7, c8, \
		c9, c10, c11, c12, c13, c14, c15, c16)			\
  do {									\
    UNION_TYPE (AVX512F_LEN, ) v;					\
    v.x = _mm512_set_ps (c1, c2, c3, c4, c5, c6, c7, c8,		\
			 c9, c10, c11, c12, c13, c14, c15, c16);	\
    TESTOP (add, +, float, ps, 0.0f);					\
    TESTOP (mul, *, float, ps, 1.0f);					\
    TESTOP (min, < v.a[i] ? r3 :, float, ps, __builtin_inff ());	\
    TESTOP (max, > v.a[i] ? r3 :, float, ps, -__builtin_inff ());	\
  } while (0)

static void
test_epi32_ps (void)
{
  TEST_EPI32 (1, 2, 3, 4, 5, 6, 6, 5, 4, 3, 2, 1, 7, 6, 5, 4);
  TEST_EPI32 (-1, 15, -1, 7, -1, 7, -1, -1, 6, 6, -1, -1, -1, -1, 7, 6);
  TEST_PS (1, 2, 3, 4, 5, 6, 6, 5, 4, 3, 2, 1, 7, 6, 5, 4);
  TEST_PS (1.25f, 2.25f, -0.25f, 4.0f, -2.0f, 4.0f, -3.0f, 2.0f,
           -0.5f, -1.0f, 1.0f, -1.0f, 1.0f, 1.0f, 2.0f, 4.0f);
}

#undef SIZE
#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"

#define TEST_EPI64(c1, c2, c3, c4, c5, c6, c7, c8) \
  do {									\
    UNION_TYPE (AVX512F_LEN, i_q) v;					\
    v.x = _mm512_set_epi64 (c1, c2, c3, c4, c5, c6, c7, c8);		\
    TESTOP (add, +, long long, epi64, 0);				\
    TESTOP (mul, *, long long, epi64, 1);				\
    TESTOP (and, &, long long, epi64, ~0LL);				\
    TESTOP (or, |, long long, epi64, 0);				\
    TESTOP (min, < v.a[i] ? r3 :, long long, epi64, __LONG_LONG_MAX__);	\
    TESTOP (max, > v.a[i] ? r3 :, long long, epi64,			\
	    -__LONG_LONG_MAX__ - 1);					\
    TESTOP (min, < (unsigned long long) v.a[i] ? r3 :,			\
	    unsigned long long, epu64, ~0ULL);				\
    TESTOP (max, > (unsigned long long) v.a[i] ? r3 :,			\
	    unsigned long long, epu64, 0);				\
  } while (0)

#define TEST_PD(c1, c2, c3, c4, c5, c6, c7, c8) \
  do {									\
    UNION_TYPE (AVX512F_LEN, d) v;					\
    v.x = _mm512_set_pd (c1, c2, c3, c4, c5, c6, c7, c8);		\
    TESTOP (add, +, double, pd, 0.0);					\
    TESTOP (mul, *, double, pd, 1.0);					\
    TESTOP (min, < v.a[i] ? r3 :, double, pd, __builtin_inf ());	\
    TESTOP (max, > v.a[i] ? r3 :, double, pd, -__builtin_inf ());	\
  } while (0)

static void
test_epi64_pd (void)
{
  TEST_EPI64 (1, 2, 3, 4, 5, 6, 6, 5);
  TEST_EPI64 (-1, 15, -1, 7, -1, 7, -1, -1);
  TEST_PD (1, 2, 3, 4, 5, 6, 6, 5);
  TEST_PD (1.25f, 2.25f, -0.25f, 4.0f, -2.0f, 4.0f, -3.0f, 2.0f);
}

void
test_512 (void)
{
  test_epi32_ps ();
  test_epi64_pd ();
}
