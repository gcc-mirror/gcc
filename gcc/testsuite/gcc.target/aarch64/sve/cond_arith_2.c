/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model" } */

#include <stdint.h>

#define TEST(DATA_TYPE, PRED_TYPE, NAME, OP)				\
  void __attribute__ ((noinline, noclone))				\
  test_##DATA_TYPE##_##PRED_TYPE##_##NAME (DATA_TYPE *__restrict x,	\
					   DATA_TYPE *__restrict y,	\
					   DATA_TYPE *__restrict z,	\
					   PRED_TYPE *__restrict pred,	\
					   int n)			\
  {									\
    for (int i = 0; i < n; ++i)						\
      x[i] = pred[i] != 1 ? y[i] OP z[i] : y[i];			\
  }

#define TEST_INT_TYPE(DATA_TYPE, PRED_TYPE) \
  TEST (DATA_TYPE, PRED_TYPE, div, /)

#define TEST_FP_TYPE(DATA_TYPE, PRED_TYPE) \
  TEST (DATA_TYPE, PRED_TYPE, add, +) \
  TEST (DATA_TYPE, PRED_TYPE, sub, -) \
  TEST (DATA_TYPE, PRED_TYPE, mul, *) \
  TEST (DATA_TYPE, PRED_TYPE, div, /)

#define TEST_ALL \
  TEST_INT_TYPE (int32_t, int8_t) \
  TEST_INT_TYPE (uint32_t, int8_t) \
  TEST_INT_TYPE (int32_t, int16_t) \
  TEST_INT_TYPE (uint32_t, int16_t) \
  TEST_INT_TYPE (int64_t, int8_t) \
  TEST_INT_TYPE (uint64_t, int8_t) \
  TEST_INT_TYPE (int64_t, int16_t) \
  TEST_INT_TYPE (uint64_t, int16_t) \
  TEST_INT_TYPE (int64_t, int32_t) \
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

/* { dg-final { scan-assembler-not {\tsel\t} } } */
