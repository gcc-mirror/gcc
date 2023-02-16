/* { dg-do run } */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */
/* { dg-require-effective-target avx512bw } */
/* { dg-require-effective-target avx512vl } */

#define AVX512BW
#define AVX512VL

#include "avx512f-helper.h"

#define FUNC_TEST_REDUCE_BASIC(opname) \
  FUNC_TEST_REDUCE_OP (, short, epi16, opname, __m128i, __mmask8) \
  FUNC_TEST_REDUCE_OP (256, short, epi16, opname, __m256i,  __mmask16) \
  FUNC_TEST_REDUCE_OP (, char, epi8, opname, __m128i, __mmask16) \
  FUNC_TEST_REDUCE_OP (256, char, epi8, opname, __m256i,  __mmask32)

#define FUNC_TEST_REDUCE_MAX_MIN(opname) \
  FUNC_TEST_REDUCE_OP (, short, epi16, opname, __m128i, __mmask8) \
  FUNC_TEST_REDUCE_OP (256, short, epi16, opname, __m256i,  __mmask16) \
  FUNC_TEST_REDUCE_OP (, char, epi8, opname, __m128i, __mmask16) \
  FUNC_TEST_REDUCE_OP (256, char, epi8, opname, __m256i,  __mmask32) \
  FUNC_TEST_REDUCE_OP (, unsigned short, epu16, opname, __m128i, __mmask8) \
  FUNC_TEST_REDUCE_OP (256, unsigned short, epu16, \
		       opname, __m256i,  __mmask16) \
  FUNC_TEST_REDUCE_OP (, unsigned char, epu8, opname, __m128i, __mmask16) \
  FUNC_TEST_REDUCE_OP (256, unsigned char, epu8, opname, __m256i,  __mmask32)

#define FUNC_TEST_REDUCE_OP(len, rtype, type, opname, argtype, masktype) \
  __attribute__((noinline, noclone)) rtype \
  test_##len##_reduce_##opname##_##type (argtype a) \
  { \
    return _mm##len##_reduce_##opname##_##type (a); \
  } \
  __attribute__((noinline, noclone)) rtype \
  test_##len##_mask_reduce_##opname##_##type (masktype u, argtype a) \
  { \
    return _mm##len##_mask_reduce_##opname##_##type (u, a); \
  }

FUNC_TEST_REDUCE_BASIC (add)
FUNC_TEST_REDUCE_BASIC (mul)
FUNC_TEST_REDUCE_BASIC (and)
FUNC_TEST_REDUCE_BASIC (or)
FUNC_TEST_REDUCE_MAX_MIN (max)
FUNC_TEST_REDUCE_MAX_MIN (min)

#define TESTOP(len, opname, op, type, suffix, neutral) \
  do {									\
    type r1 = _mm##len##_reduce_##opname##_##suffix (v.x);			\
    type r2 = test_##len##_reduce_##opname##_##suffix (v.x);			\
    type r3 = neutral;							\
    if (r1 != r2)							\
      __builtin_abort ();						\
    for (int i = 0; i < SIZE; i++)					\
      r3 = r3 op v.a[i];						\
    if (r1 != r3)							\
      __builtin_abort ();						\
    type r4 = _mm##len##_mask_reduce_##opname##_##suffix (MASK_VALUE, v.x);	\
    type r5 = test_##len##_mask_reduce_##opname##_##suffix (MASK_VALUE, v.x);	\
    if (r4 != r5)							\
      __builtin_abort ();						\
    r3 = neutral;							\
    for (int i = 0; i < SIZE; i++)					\
      if (MASK_VALUE & (1 << i))					\
	r3 = r3 op v.a[i];						\
    if (r4 != r3)							\
      __builtin_abort ();						\
    type r6 = _mm##len##_mask_reduce_##opname##_##suffix (0, v.x);		\
    type r7 = test_##len##_mask_reduce_##opname##_##suffix (0, v.x);		\
    if (r6 != r7 || r6 != neutral)					\
      __builtin_abort ();						\
  } while (0)

#undef AVX512F_LEN
#define AVX512F_LEN 128

#undef SIZE
#define SIZE (AVX512F_LEN / 8)
#include "avx512f-mask-type.h"

#define TEST_128_EPI8(c1, c2, c3, c4, c5, c6, c7, c8, \
		   c9, c10, c11, c12, c13, c14, c15, c16)		\
  do {									\
    UNION_TYPE (AVX512F_LEN, i_b) v;					\
    v.x = _mm_set_epi8 (c1, c2, c3, c4, c5, c6, c7, c8,		\
			    c9, c10, c11, c12, c13, c14, c15, c16);	\
    TESTOP (, add, +, char, epi8, 0);					\
    TESTOP (, mul, *, char, epi8, 1);					\
    TESTOP (, and, &, char, epi8, (char) ~0);					\
    TESTOP (, or, |, char, epi8, 0);					\
    TESTOP (, min, < v.a[i] ? r3 :, char, epi8, __SCHAR_MAX__);		\
    TESTOP (, max, > v.a[i] ? r3 :, char, epi8, -__SCHAR_MAX__ - 1);	\
    TESTOP (, min, < (unsigned char) v.a[i] ? r3 :, unsigned char, epu8, (unsigned char) ~0U);	\
    TESTOP (, max, > (unsigned char) v.a[i] ? r3 :, unsigned char, epu8, 0); \
  } while (0)

static void
test_128_epi8 (void)
{
  TEST_128_EPI8 (1, 2, 3, 4, 5, 6, 6, 5, 4, 3, 2, 1, 7, 6, 5, 4);
  TEST_128_EPI8 (-1, 15, -1, 7, -1, 7, -1, -1, 6, 6, -1, -1, -1, -1, 7, 6);
}

#undef SIZE
#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

#define TEST_128_EPI16(c1, c2, c3, c4, c5, c6, c7, c8) \
  do {									\
    UNION_TYPE (AVX512F_LEN, i_w) v;					\
    v.x = _mm_set_epi16 (c1, c2, c3, c4, c5, c6, c7, c8);		\
    TESTOP (, add, +, short, epi16, 0);					\
    TESTOP (, mul, *, short, epi16, 1);					\
    TESTOP (, and, &, short, epi16, (short) ~0);			\
    TESTOP (, or, |, short, epi16, 0);					\
    TESTOP (, min, < v.a[i] ? r3 :, short, epi16, __SHRT_MAX__);	\
    TESTOP (, max, > v.a[i] ? r3 :, short, epi16, -__SHRT_MAX__ - 1);	\
    TESTOP (, min, < (unsigned short) v.a[i] ? r3 :, unsigned short, epu16,(unsigned short) ~0U);	\
    TESTOP (, max, > (unsigned short) v.a[i] ? r3 :, unsigned short, epu16, 0);	\
  } while (0)

static void
test_128_epi16 (void)
{
  TEST_128_EPI16 (1, 2, 3, 4, 5, 6, 6, 5);
  TEST_128_EPI16 (-1, 15, -1, 7, -1, 7, -1, -1);
}

void
test_128 (void)
{
  test_128_epi8 ();
  test_128_epi16 ();
}

#undef AVX512F_LEN
#define AVX512F_LEN 256

#undef SIZE
#define SIZE (AVX512F_LEN / 8)
#include "avx512f-mask-type.h"

#define TEST_256_EPI8(c1, c2, c3, c4, c5, c6, c7, c8, \
		   c9, c10, c11, c12, c13, c14, c15, c16,		\
		   c17, c18, c19, c20, c21, c22, c23, c24,		\
		   c25, c26, c27, c28, c29, c30, c31, c32)		\
  do {									\
    UNION_TYPE (AVX512F_LEN, i_b) v;					\
    v.x = _mm256_set_epi8 (c1, c2, c3, c4, c5, c6, c7, c8,		\
			    c9, c10, c11, c12, c13, c14, c15, c16,	\
			    c17, c18, c19, c20, c21, c22, c23, c24,	\
			    c25, c26, c27, c28, c29, c30, c31, c32);	\
    TESTOP (256, add, +, char, epi8, 0);				\
    TESTOP (256, mul, *, char, epi8, 1);				\
    TESTOP (256, and, &, char, epi8, (char) ~0);			\
    TESTOP (256, or, |, char, epi8, 0);					\
    TESTOP (256, min, < v.a[i] ? r3 :, char, epi8, __SCHAR_MAX__);	\
    TESTOP (256, max, > v.a[i] ? r3 :, char, epi8, -__SCHAR_MAX__ - 1);	\
    TESTOP (256, min, < (unsigned char) v.a[i] ? r3 :,			\
	    unsigned char, epu8, (unsigned char)~0U);			\
    TESTOP (256, max, > (unsigned char) v.a[i] ? r3 :,			\
	    unsigned char, epu8, 0);	\
  } while (0)

static void
test_256_epi8 (void)
{
  TEST_256_EPI8 (1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 12, 11, 10, 9, 9, 7, 6, 5, 4, 3, 2, 1, 7, 6, 5, 4, 7, 10, 11, 12);
  TEST_256_EPI8 (-1, 15, -1, 7, -1, 7, -1, -1, 6, 6, -1, -1, -1, -1, 7, 6, -1, 30, -1, 28, -1, 26, -1, 24, -1, 22, -1, -1, -1, -1, 17, 16);
}

#undef SIZE
#define SIZE (AVX512F_LEN / 16)
#include "avx512f-mask-type.h"

#define TEST_256_EPI16(c1, c2, c3, c4, c5, c6, c7, c8, \
		   c9, c10, c11, c12, c13, c14, c15, c16)		\
  do {									\
    UNION_TYPE (AVX512F_LEN, i_w) v;					\
    v.x = _mm256_set_epi16 (c1, c2, c3, c4, c5, c6, c7, c8,		\
			    c9, c10, c11, c12, c13, c14, c15, c16);	\
    TESTOP (256, add, +, short, epi16, 0);				\
    TESTOP (256, mul, *, short, epi16, 1);				\
    TESTOP (256, and, &, short, epi16, (short) ~0);			\
    TESTOP (256, or, |, short, epi16, 0);				\
    TESTOP (256, min, < v.a[i] ? r3 :, short, epi16, __SHRT_MAX__);	\
    TESTOP (256, max, > v.a[i] ? r3 :, short, epi16, -__SHRT_MAX__ - 1);\
    TESTOP (256, min, < (unsigned short) v.a[i] ? r3 :,			\
	    unsigned short, epu16, (unsigned short) ~0U);		\
    TESTOP (256, max, > (unsigned short) v.a[i] ? r3 :,			\
	    unsigned short, epu16, 0);					\
  } while (0)

static void
test_256_epi16 (void)
{
  TEST_256_EPI16 (9, 7, 6, 5, 4, 3, 2, 1, 7, 6, 5, 4, 7, 10, 11, 12);
  TEST_256_EPI16 (-1, 15, -1, 7, -1, 7, -1, -1, 6, 6, -1, -1, -1, -1, 7, 6);
}

void
test_256 (void)
{
  test_256_epi8 ();
  test_256_epi16 ();
}
