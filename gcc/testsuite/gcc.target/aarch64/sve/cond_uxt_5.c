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
      r[i] = a[i] > 20 ? b[i] & CONST : b[i];			\
  }

#define TEST_ALL(T)			\
  T (int32_t, uint16_t, 0xff, 3)	\
					\
  T (int64_t, uint16_t, 0xff, 5)	\
  T (int64_t, uint32_t, 0xff, 5)	\
  T (int64_t, uint32_t, 0xffff, 5)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler {\tld1h\t(z[0-9]+)\.s, p[0-7]/z, \[x2[],][^L]*\tuxtb\t\1\.h, p[0-7]/m, \1\.h\n} } } */

/* { dg-final { scan-assembler {\tld1h\t(z[0-9]+)\.d, p[0-7]/z, \[x2[],][^L]*\tuxtb\t\1\.h, p[0-7]/m, \1\.h\n} } } */
/* { dg-final { scan-assembler {\tld1w\t(z[0-9]+)\.d, p[0-7]/z, \[x2[],][^L]*\tuxtb\t\1\.s, p[0-7]/m, \1\.s\n} } } */
/* { dg-final { scan-assembler {\tld1w\t(z[0-9]+)\.d, p[0-7]/z, \[x2[],][^L]*\tuxth\t\1\.s, p[0-7]/m, \1\.s\n} } } */

/* { dg-final { scan-assembler-not {\tmov\tz[^\n]*z} } } */
/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-not {\tsel\t} } } */
