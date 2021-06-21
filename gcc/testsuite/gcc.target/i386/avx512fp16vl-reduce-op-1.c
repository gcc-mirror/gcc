/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */

static void do_test (void);

#define DO_TEST do_test
#define AVX512FP16

#include <immintrin.h>
#include "avx512-check.h"

__m256h a1 = { -39.3f16, -180.9f16, 13.4f16, 35.4f16, -41.1f16, -14.4f16, 24.5f16, 53.54f16,
	       238.4f16, -134.8f16, 24.5f16, 35.6f16, -346.7f16, -43.4f16, -535.3f16, 324.7f16 };
__m256h a2 = { 82.5f16, 21.4f16, 24.4f16, 53.4f16, 23.5f16, -24.4f16, -34.5f16, -32.5f16,
	       23.6f16, -13.4f16, 24.5f16, 35.5f16, -34.4f16, -24.5f16, -34.5f16, 13.5f16 };

__m128h b1 = { 1.25f16, 2.25f16, -0.25f16, 4.0f16, -2.0f16, 4.0f16, -3.0f16, 2.0f16 };
__m128h b2 = { -0.5f16, -1.0f16, 1.0f16, -1.0f16, 1.0f16, 1.0f16, 2.0f16, 4.0f16 };
__m128h b3 = { 1.25f16, 2.25f16, -4.25f16, 4.0f16, -2.4f16, 4.0f16, -3.0f, 2.0f16 };
__m128h b4 = { -4.5f16, 7.6f16, 0.7f16, -8.2f16, 2.1f16, 2.4f16, -2.0f16, 1.4f16 };

__attribute__((noinline, noclone)) _Float16
test_reduce_256_add_ph (__m256h a)
{
  return _mm256_reduce_add_ph (a);
}

__attribute__((noinline, noclone)) _Float16
test_reduce_256_mul_ph (__m256h a)
{
  return _mm256_reduce_mul_ph (a);
}

__attribute__((noinline, noclone)) _Float16
test_reduce_256_max_ph (__m256h a)
{
  return _mm256_reduce_max_ph (a);
}

__attribute__((noinline, noclone)) _Float16
test_reduce_256_min_ph (__m256h a)
{
  return _mm256_reduce_min_ph (a);
}

__attribute__((noinline, noclone)) _Float16
test_reduce_add_ph (__m128h b)
{
  return _mm_reduce_add_ph (b);
}

__attribute__((noinline, noclone)) _Float16
test_reduce_mul_ph (__m128h b)
{
  return _mm_reduce_mul_ph (b);
}

__attribute__((noinline, noclone)) _Float16
test_reduce_max_ph (__m128h b)
{
  return _mm_reduce_max_ph (b);
}

__attribute__((noinline, noclone)) _Float16
test_reduce_min_ph (__m128h b)
{
  return _mm_reduce_min_ph (b);
}

#define SIZE 16
#define REF_ADDMUL(op, a)				\
  __m128h __a1 = _mm_setzero_ph ();			\
  for (int i = 0; i < 8; i++) {				\
    __a1[i] = (_Float16) a[i] op (_Float16) a[i + 8];	\
  }							\
  _Float16 __c0 = __a1[0] op __a1[4];			\
  _Float16 __c1 = __a1[1] op __a1[5];			\
  _Float16 __c2 = __a1[2] op __a1[6];			\
  _Float16 __c3 = __a1[3] op __a1[7];			\
  _Float16 __d0 = __c0 op __c2;				\
  _Float16 __d1 = __c1 op __c3;				\
  _Float16 __e0 = __d0 op __d1;				\
  r3 = __e0

#define TESTOP(opname, op, a)				\
  do {							\
    _Float16 r1 = _mm256_reduce_##opname##_ph (a);	\
    _Float16 r2 = test_reduce_256_##opname##_ph (a);	\
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
test_256_addmul_ph (void)
{
  TEST_ADDMUL_PH (a1);
  TEST_ADDMUL_PH (a2);
}

#undef TESTOP
#define TESTOP(opname, op, a)				\
  do {							\
    _Float16 r1 = _mm256_reduce_##opname##_ph (a);	\
    _Float16 r2 = test_reduce_256_##opname##_ph (a);	\
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
test_256_minmax_ph (void)
{
  TEST_MINMAX_PH (a1);
  TEST_MINMAX_PH (a2);
}

static void
test_256_ph (void)
{
   test_256_addmul_ph ();
   test_256_minmax_ph ();
}

#undef SIZE
#define SIZE 8

#undef REF_ADDMUL
#define REF_ADDMUL(op, a)			\
  _Float16 __c0 = a[0] op a[4];			\
  _Float16 __c1 = a[1] op a[5];			\
  _Float16 __c2 = a[2] op a[6];			\
  _Float16 __c3 = a[3] op a[7];			\
  _Float16 __d0 = __c0 op __c2;			\
  _Float16 __d1 = __c1 op __c3;			\
  _Float16 __e0 = __d0 op __d1;			\
  r3 = __e0

#undef TESTOP
#define TESTOP(opname, op, a)				\
  do {							\
    _Float16 r1 = _mm_reduce_##opname##_ph (a);		\
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

#undef TEST_ADDMUL_PH
#define TEST_ADDMUL_PH(a)			\
  do {						\
    TESTOP (add, +, a);				\
    TESTOP (mul, *, a);				\
  } while (0)

static void
test_128_addmul_ph (void)
{
  TEST_ADDMUL_PH (b1);
  TEST_ADDMUL_PH (b2);
  TEST_ADDMUL_PH (b3);
  TEST_ADDMUL_PH (b4);
}

#undef TESTOP
#define TESTOP(opname, op, b)				\
  do {							\
    _Float16 r1 = _mm_reduce_##opname##_ph (b);		\
    _Float16 r2 = test_reduce_##opname##_ph (b);	\
    _Float16 r3 = b[0];					\
    if (r1 != r2) {					\
      __builtin_abort ();				\
    }							\
    for (int i = 1; i < SIZE; i++)			\
      r3 = r3 op b[i];					\
    if (r1 != r3) {					\
      __builtin_abort ();				\
    }							\
  } while (0)

#undef TEST_MINMAX_PH
#define TEST_MINMAX_PH(b)			\
  do {						\
    TESTOP (min, < b[i] ? r3 :, b);		\
    TESTOP (max, > b[i] ? r3 :, b);		\
  } while (0)

static void
test_128_minmax_ph (void)
{
  TEST_MINMAX_PH (b1);
  TEST_MINMAX_PH (b2);
  TEST_MINMAX_PH (b3);
  TEST_MINMAX_PH (b4);
}

static void
test_128_ph (void)
{
  test_128_addmul_ph ();
  test_128_minmax_ph ();
}

static void
do_test (void)
{
  test_256_ph ();
  test_128_ph ();
}


#undef SIZE
#undef REF_ADDMUL
#undef TESTOP
#undef TEST_ADDMUL_PH
#undef TEST_MINMAX_PH
