/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16" } */

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16

#include <immintrin.h>
#include "avx512-check.h"

__m512h a1 = { -39.3f16, -180.9f16, 13.4f16, 35.4f16, -41.1f16, -14.4f16, 24.5f16, 53.54f16,
		238.4f16, -134.8f16, 24.5f16, 35.6f16, -346.7f16, -43.4f16, -535.3f16, 324.7f16,
		82.5f16, 21.4f16, 24.4f16, 53.4f16, 23.5f16, -24.4f16, -34.5f16, -32.5f16,
		23.6f16, -13.4f16, 24.5f16, 35.5f16, -34.4f16, -24.5f16, -34.5f16, 13.5f16 };

__m512h a2 = { 1.25f16, 2.25f16, -0.25f16, 4.0f16, -2.0f16, 4.0f16, -3.0f16, 2.0f16,
	       -0.5f16, -1.0f16, 1.0f16, -1.0f16, 1.0f16, 1.0f16, 2.0f16, 4.0f16,
	       1.25f16, 2.25f16, -4.25f16, 4.0f16, -2.4f16, 4.0f16, -3.0f, 2.0f16,
	       -4.5f16, 7.6f16, 0.7f16, -8.2f16, 2.1f16, 2.4f16, -2.0f16, 19.4f16 };

__attribute__((noinline, noclone)) _Float16
test_reduce_add_ph (__m512h a)
{
  return _mm512_reduce_add_ph (a);
}

__attribute__((noinline, noclone)) _Float16
test_reduce_mul_ph (__m512h a)
{
  return _mm512_reduce_mul_ph (a);
}

__attribute__((noinline, noclone)) _Float16
test_reduce_max_ph (__m512h a)
{
  return _mm512_reduce_max_ph (a);
}

__attribute__((noinline, noclone)) _Float16
test_reduce_min_ph (__m512h a)
{
  return _mm512_reduce_min_ph (a);
}

#define SIZE 32
#define REF_ADDMUL(op, a)					\
  __m256h __a1 = _mm256_setzero_ph ();				\
  for (int i =0; i < 16; i++) {					\
    __a1[i] = (_Float16) a[i] op (_Float16) a[i + 16];		\
  }								\
  __m128h __a2 = _mm_setzero_ph ();				\
  for (int i =0; i < 8; i++) {					\
    __a2[i] = (_Float16) __a1[i] op (_Float16) __a1[i + 8];	\
  }								\
  _Float16 __c0 = __a2[0] op __a2[4];				\
  _Float16 __c1 = __a2[1] op __a2[5];				\
  _Float16 __c2 = __a2[2] op __a2[6];				\
  _Float16 __c3 = __a2[3] op __a2[7];				\
  _Float16 __d0 = __c0 op __c2;					\
  _Float16 __d1 = __c1 op __c3;					\
  _Float16 __e0 = __d0 op __d1;					\
  r3 = __e0

#define TESTOP(opname, op, a)				\
  do {							\
    _Float16 r1 = _mm512_reduce_##opname##_ph (a);	\
    _Float16 r2 = test_reduce_##opname##_ph (a);	\
    _Float16 r3 = a[0];					\
    if (r1 != r2) {					\
      __builtin_abort ();				\
    }							\
    REF_ADDMUL (op, a);					\
    if (r1 != r3) {					\
      __builtin_abort ();				\
    }							\
  } while (0)

#define TEST_ADDMUL_PH(a)			\
  do {						\
    TESTOP (add, +, a);				\
    TESTOP (mul, *, a);				\
  } while (0)

  static void
  test_512_addmul_ph (void)
  {
    TEST_ADDMUL_PH (a1);
    TEST_ADDMUL_PH (a2);
  }

#undef TESTOP
#define TESTOP(opname, op, a)				\
  do {							\
    _Float16 r1 = _mm512_reduce_##opname##_ph (a);	\
    _Float16 r2 = test_reduce_##opname##_ph (a);	\
    _Float16 r3 = a[0];					\
    if (r1 != r2) {					\
      __builtin_abort ();				\
    }							\
    for (int i = 1; i < SIZE; i++)			\
      r3 = r3 op a[i];					\
    if (r1 != r3) {					\
      __builtin_abort ();				\
    }							\
  } while (0)

#define TEST_MINMAX_PH(a)			\
  do {						\
    TESTOP (min, < a[i] ? r3 :, a);		\
    TESTOP (max, > a[i] ? r3 :, a);		\
  } while (0)

static void
test_512_minmax_ph (void)
{
  TEST_MINMAX_PH (a1);
  TEST_MINMAX_PH (a2);
}

static void
do_test (void)
{
  test_512_addmul_ph();
  test_512_minmax_ph();
}

#undef SIZE
#undef REF_ADDMUL
#undef TESTOP 
#undef TEST_ADDMUL_PH
#undef TEST_MINMAX_PH
