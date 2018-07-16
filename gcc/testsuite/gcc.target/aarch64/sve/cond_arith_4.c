/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define TEST(TYPE, NAME, OP)						\
  void __attribute__ ((noinline, noclone))				\
  test_##TYPE##_##NAME (TYPE *__restrict x,				\
			TYPE *__restrict y,				\
			TYPE z1, TYPE z2,				\
			TYPE *__restrict pred, int n)			\
  {									\
    for (int i = 0; i < n; i += 2)					\
      {									\
	x[i] = (pred[i] != 1 ? y[i] OP z1 : y[i]);			\
	x[i + 1] = (pred[i + 1] != 1 ? y[i + 1] OP z2 : y[i + 1]);	\
      }									\
  }

#define TEST_INT_TYPE(TYPE) \
  TEST (TYPE, div, /)

#define TEST_FP_TYPE(TYPE) \
  TEST (TYPE, add, +) \
  TEST (TYPE, sub, -) \
  TEST (TYPE, mul, *) \
  TEST (TYPE, div, /)

#define TEST_ALL \
  TEST_INT_TYPE (int32_t) \
  TEST_INT_TYPE (uint32_t) \
  TEST_INT_TYPE (int64_t) \
  TEST_INT_TYPE (uint64_t) \
  TEST_FP_TYPE (float) \
  TEST_FP_TYPE (double)

TEST_ALL

/* { dg-final { scan-assembler-times {\tsdiv\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tudiv\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tsdiv\tz[0-9]+\.d, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tudiv\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfdiv\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]/z,} 12 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7],} 6 } } */

/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]/z,} 12 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7],} 6 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
