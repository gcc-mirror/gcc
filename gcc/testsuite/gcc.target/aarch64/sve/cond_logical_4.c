/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define bit_and(A, B) ((A) & (B))
#define bit_or(A, B) ((A) | (B))
#define bit_xor(A, B) ((A) ^ (B))
#define bit_bic(A, B) ((A) & ~(B))

#define DEF_LOOP(TYPE, OP)				\
  void __attribute__ ((noinline, noclone))		\
  test_##TYPE##_##OP (TYPE *__restrict r,		\
		      TYPE *__restrict a,		\
		      TYPE *__restrict b,		\
		      TYPE *__restrict c, int n)	\
  {							\
    for (int i = 0; i < n; ++i)				\
      r[i] = a[i] < 20 ? OP (b[i], c[i]) : 42;		\
  }

#define TEST_TYPE(T, TYPE) \
  T (TYPE, bit_and) \
  T (TYPE, bit_or) \
  T (TYPE, bit_xor) \
  T (TYPE, bit_bic)

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

/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.b, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.h, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tand\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.b, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.h, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\torr\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.b, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.h, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\teor\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-times {\tbic\tz[0-9]+\.b, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tbic\tz[0-9]+\.h, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tbic\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tbic\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^,]*z} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-times {\tsel\t} 32 } } */
