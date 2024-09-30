/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define DEF_LOOP(TYPE, NAME, OP, CONST)				\
  void __attribute__ ((noipa))					\
  test_##TYPE##_##NAME##_##CONST (TYPE *__restrict r, 		\
				  TYPE *__restrict a,		\
				  TYPE *__restrict b,		\
				  TYPE *__restrict pred, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      r[i] = pred[i] != 1 ? a[i] OP b[i] * -CONST : a[i];	\
  }

#define TEST_COUNT(T, TYPE, CONST) \
  T (TYPE, add, +, CONST) \
  T (TYPE, sub, -, CONST)

#define TEST_TYPE(T, TYPE, CONST) \
  TEST_COUNT (T, TYPE, 2) \
  TEST_COUNT (T, TYPE, 4) \
  TEST_COUNT (T, TYPE, CONST)

#define TEST_ALL(T) \
  TEST_TYPE (T, uint8_t, 0x80) \
  TEST_TYPE (T, uint16_t, 0x8000) \
  TEST_TYPE (T, uint32_t, 0x80000000) \
  TEST_TYPE (T, uint64_t, 0x8000000000000000ULL)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.b, z[0-9]+\.b, #2\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.b, z[0-9]+\.b, #7\n} 2 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.h, z[0-9]+\.h, #2\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.h, z[0-9]+\.h, #15\n} 2 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.s, z[0-9]+\.s, #2\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.s, z[0-9]+\.s, #31\n} 2 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.d, z[0-9]+\.d, #2\n} 2 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.d, z[0-9]+\.d, #63\n} 2 } } */

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 4 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */

/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsub\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^,]*z} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
