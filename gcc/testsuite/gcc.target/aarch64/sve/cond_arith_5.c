/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model" } */

#include <stdint.h>

#define TEST(DATA_TYPE, OTHER_TYPE, NAME, OP)				\
  void __attribute__ ((noinline, noclone))				\
  test_##DATA_TYPE##_##OTHER_TYPE##_##NAME (DATA_TYPE *__restrict x,	\
					    DATA_TYPE *__restrict y,	\
					    DATA_TYPE z1, DATA_TYPE z2,	\
					    DATA_TYPE *__restrict pred,	\
					    OTHER_TYPE *__restrict foo,	\
					    int n)			\
  {									\
    for (int i = 0; i < n; i += 2)					\
      {									\
	x[i] = (pred[i] != 1 ? y[i] OP z1 : y[i]);			\
	x[i + 1] = (pred[i + 1] != 1 ? y[i + 1] OP z2 : y[i + 1]);	\
	foo[i] += 1;							\
	foo[i + 1] += 2;						\
      }									\
  }

#define TEST_INT_TYPE(DATA_TYPE, OTHER_TYPE) \
  TEST (DATA_TYPE, OTHER_TYPE, div, /)

#define TEST_FP_TYPE(DATA_TYPE, OTHER_TYPE) \
  TEST (DATA_TYPE, OTHER_TYPE, add, +) \
  TEST (DATA_TYPE, OTHER_TYPE, sub, -) \
  TEST (DATA_TYPE, OTHER_TYPE, mul, *) \
  TEST (DATA_TYPE, OTHER_TYPE, div, /)

#define TEST_ALL \
  TEST_INT_TYPE (int32_t, int8_t) \
  TEST_INT_TYPE (int32_t, int16_t) \
  TEST_INT_TYPE (uint32_t, int8_t) \
  TEST_INT_TYPE (uint32_t, int16_t) \
  TEST_INT_TYPE (int64_t, int8_t) \
  TEST_INT_TYPE (int64_t, int16_t) \
  TEST_INT_TYPE (int64_t, int32_t) \
  TEST_INT_TYPE (uint64_t, int8_t) \
  TEST_INT_TYPE (uint64_t, int16_t) \
  TEST_INT_TYPE (uint64_t, int32_t) \
  TEST_FP_TYPE (float, int8_t) \
  TEST_FP_TYPE (float, int16_t) \
  TEST_FP_TYPE (double, int8_t) \
  TEST_FP_TYPE (double, int16_t) \
  TEST_FP_TYPE (double, int32_t)

TEST_ALL

/* { dg-final { scan-assembler-times {\tsdiv\tz[0-9]+\.s, p[0-7]/m,} 6 } } */
/* { dg-final { scan-assembler-times {\tudiv\tz[0-9]+\.s, p[0-7]/m,} 6 } } */
/* { dg-final { scan-assembler-times {\tsdiv\tz[0-9]+\.d, p[0-7]/m,} 14 } } */
/* { dg-final { scan-assembler-times {\tudiv\tz[0-9]+\.d, p[0-7]/m,} 14 } } */

/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.s, p[0-7]/m,} 6 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.d, p[0-7]/m,} 14 } } */

/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.s, p[0-7]/m,} 6 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.d, p[0-7]/m,} 14 } } */

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, p[0-7]/m,} 6 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.d, p[0-7]/m,} 14 } } */

/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.s, p[0-7]/m,} 6 } } */
/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.d, p[0-7]/m,} 14 } } */

/* The load XFAILs for fixed-length SVE account for extra loads from the
   constant pool.  */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.b, p[0-7]/z,} 12 { xfail { aarch64_sve && { ! vect_variable_length } } } } } */
/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.b, p[0-7],} 12 } } */

/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.h, p[0-7]/z,} 12 { xfail { aarch64_sve && { ! vect_variable_length } } } } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.h, p[0-7],} 12 } } */

/* 72 for x operations, 6 for foo operations.  */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z,} 78 { xfail { aarch64_sve && { ! vect_variable_length } } } } } */
/* 36 for x operations, 6 for foo operations.  */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7],} 42 } } */

/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z,} 168 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7],} 84 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
