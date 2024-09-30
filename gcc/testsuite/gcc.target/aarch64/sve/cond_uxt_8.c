/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define DEF_LOOP(TYPE1, TYPE2, CONST, COUNT)			\
  void __attribute__ ((noipa))					\
  test_##CONST##_##TYPE1##_##TYPE2 (TYPE2 *restrict r,		\
				    TYPE1 *restrict a,		\
				    TYPE2 *restrict b)		\
  {								\
    for (int i = 0; i < COUNT; ++i)				\
      r[i] = a[i] > 20 ? b[i] & CONST : 0;			\
  }

#define TEST_ALL(T)			\
  T (int32_t, uint16_t, 0xff, 30)	\
					\
  T (int64_t, uint16_t, 0xff, 50)	\
  T (int64_t, uint32_t, 0xff, 50)	\
  T (int64_t, uint32_t, 0xffff, 50)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tuxtb\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuxtb\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuxth\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */

/* { dg-final { scan-assembler {\tmovprfx\tz[^,]*, p[0-7]/z} } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^\n]*z} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
