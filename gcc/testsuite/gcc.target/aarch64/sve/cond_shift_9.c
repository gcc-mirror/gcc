/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define DEF_LOOP(TYPE, NAME, OP)					\
  void __attribute__ ((noipa))						\
  test_##TYPE##_##NAME (TYPE *__restrict r, TYPE *__restrict a,		\
			TYPE *__restrict b, TYPE *__restrict c, int n)	\
  {									\
    for (int i = 0; i < n; ++i)						\
      r[i] = a[i] > 20 ? b[i] OP c[i] : 0;				\
  }

#define TEST_TYPE(T, TYPE) \
  T (TYPE, shl, <<) \
  T (TYPE, shr, >>)

#define TEST_ALL(T) \
  TEST_TYPE (T, int32_t) \
  TEST_TYPE (T, uint32_t) \
  TEST_TYPE (T, int64_t) \
  TEST_TYPE (T, uint64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tlslr?\tz[0-9]+\.s, p[0-7]/m,} 2 } } */
/* { dg-final { scan-assembler-times {\tlslr?\tz[0-9]+\.d, p[0-7]/m,} 2 } } */

/* { dg-final { scan-assembler-times {\tasrr?\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tasrr?\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tlsrr?\tz[0-9]+\.s, p[0-7]/m,} 1 } } */
/* { dg-final { scan-assembler-times {\tlsrr?\tz[0-9]+\.d, p[0-7]/m,} 1 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.s, p[0-7]/z, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.d, p[0-7]/z, z[0-9]+\.d\n} 4 } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^,]*z} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
