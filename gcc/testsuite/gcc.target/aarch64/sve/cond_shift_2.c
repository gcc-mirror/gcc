/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define DEF_LOOP(TYPE, NAME, OP)					\
  void __attribute__ ((noipa))						\
  test_##TYPE##_##NAME (TYPE *__restrict r, TYPE *__restrict a,		\
			TYPE *__restrict b, int n)			\
  {									\
    for (int i = 0; i < n; ++i)						\
      r[i] = a[i] > 20 ? b[i] OP 3 : a[i];				\
  }

#define TEST_TYPE(T, TYPE) \
  T (TYPE, shl, <<) \
  T (TYPE, shr, >>)

#define TEST_ALL(T) \
  TEST_TYPE (T, int8_t) \
  TEST_TYPE (T, uint8_t) \
  TEST_TYPE (T, int16_t) \
  TEST_TYPE (T, uint16_t) \
  TEST_TYPE (T, int32_t) \
  TEST_TYPE (T, uint32_t) \
  TEST_TYPE (T, int64_t) \
  TEST_TYPE (T, uint64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.b, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.h, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tasr\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.b, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.h, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b\n} 4 } } */
/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d\n} 4 } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^,]*z} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
