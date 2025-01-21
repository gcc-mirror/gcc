/* { dg-do compile } */
/* { dg-additional-options "-O3 -ffast-math" } */

#include "arm_sve.h"

#pragma GCC target "+sve2+faminmax"

#define TEST_FAMAX(TYPE)						\
  void fn_famax_##TYPE (TYPE * restrict a,				\
			TYPE * restrict b,				\
			TYPE * restrict c,				\
			int n) {					\
    for (int i = 0; i < n; i++) {					\
      TYPE temp1 = __builtin_fabs (a[i]);				\
      TYPE temp2 = __builtin_fabs (b[i]);				\
      c[i] = __builtin_fmax (temp1, temp2);				\
    }									\
  }									\

#define TEST_FAMIN(TYPE)						\
  void fn_famin_##TYPE (TYPE * restrict a,				\
			TYPE * restrict b,				\
			TYPE * restrict c,				\
			int n) {					\
    for (int i = 0; i < n; i++) {					\
      TYPE temp1 = __builtin_fabs (a[i]);				\
      TYPE temp2 = __builtin_fabs (b[i]);				\
      c[i] = __builtin_fmin (temp1, temp2);				\
    }									\
  }									\

TEST_FAMAX (float16_t)
TEST_FAMAX (float32_t)
TEST_FAMAX (float64_t)
TEST_FAMIN (float16_t)
TEST_FAMIN (float32_t)
TEST_FAMIN (float64_t)

/* { dg-final { scan-assembler-times {\tfamax\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfamax\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfamax\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfamin\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfamin\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfamin\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
