/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define DEF_LOOP(TYPE, NAME, OP)			\
  void __attribute__ ((noipa))				\
  test_##TYPE##_##NAME (TYPE *__restrict r,		\
			TYPE *__restrict a,		\
			TYPE *__restrict b, TYPE c,	\
			TYPE *__restrict pred, int n)	\
  {							\
    for (int i = 0; i < n; ++i)				\
      r[i] = pred[i] != 1 ? a[i] OP b[i] * c : c;	\
  }

#define TEST_TYPE(T, TYPE) \
  T (TYPE, add, +) \
  T (TYPE, sub, -)

#define TEST_ALL(T) \
  TEST_TYPE (T, uint8_t) \
  TEST_TYPE (T, uint16_t) \
  TEST_TYPE (T, uint32_t) \
  TEST_TYPE (T, uint64_t) \
  TEST_TYPE (T, _Float16) \
  TEST_TYPE (T, float) \
  TEST_TYPE (T, double)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tmad\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tmad\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tmad\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tmad\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tmsb\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tmsb\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tmsb\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tmsb\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfmad\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfmad\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfmad\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tfmsb\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfmsb\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tfmsb\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+, z[0-9]+\n} 14 } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^,]*z} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
