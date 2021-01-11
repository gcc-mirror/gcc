/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define DEF_LOOP(TYPE1, TYPE2, COUNT)				\
  void __attribute__ ((noipa))					\
  test_##TYPE1##_##TYPE2##_##TYPE3 (TYPE2 *restrict r,		\
				    TYPE1 *restrict pred,	\
				    TYPE2 *restrict a)		\
  {								\
    for (int i = 0; i < COUNT; ++i)				\
      if (pred[i])						\
	r[i] = a[i] / 16;					\
  }

#define TEST_ALL(T) \
  T (int16_t, int8_t, 7) \
  T (int32_t, int8_t, 3) \
  T (int32_t, int16_t, 3) \
  T (int64_t, int8_t, 5) \
  T (int64_t, int16_t, 5) \
  T (int64_t, int32_t, 5)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tasrd\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, #4\n} 3 } } */
/* { dg-final { scan-assembler-times {\tasrd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #4\n} 2 } } */
/* { dg-final { scan-assembler-times {\tasrd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #4\n} 1 } } */
