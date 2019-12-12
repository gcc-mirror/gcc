/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define NUM_ELEMS(TYPE) (320 / sizeof (TYPE))

#define DEF_LOOP(TYPE, CONST)					\
  void __attribute__ ((noipa))					\
  test_##CONST##_##TYPE (TYPE *restrict r, TYPE *restrict a,	\
			 TYPE *restrict b)			\
  {								\
    for (int i = 0; i < NUM_ELEMS (TYPE); ++i)			\
      r[i] = a[i] > 20 ? b[i] & CONST : 0;			\
  }

#define TEST_ALL(T)			\
  T (uint16_t, 0xff)			\
					\
  T (uint32_t, 0xff)			\
  T (uint32_t, 0xffff)			\
					\
  T (uint64_t, 0xff)			\
  T (uint64_t, 0xffff)			\
  T (uint64_t, 0xffffffff)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.h), (p[0-7])/z, z[0-9]+\.h\n\tuxtb\t\1, \2/m, z[0-9]+\.h\n} } } */

/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.s), (p[0-7])/z, z[0-9]+\.s\n\tuxtb\t\1, \2/m, z[0-9]+\.s\n} } } */
/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.s), (p[0-7])/z, z[0-9]+\.s\n\tuxth\t\1, \2/m, z[0-9]+\.s\n} } } */

/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.d), (p[0-7])/z, z[0-9]+\.d\n\tuxtb\t\1, \2/m, z[0-9]+\.d\n} } } */
/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.d), (p[0-7])/z, z[0-9]+\.d\n\tuxth\t\1, \2/m, z[0-9]+\.d\n} } } */
/* { dg-final { scan-assembler {\tmovprfx\t(z[0-9]+\.d), (p[0-7])/z, z[0-9]+\.d\n\tuxtw\t\1, \2/m, z[0-9]+\.d\n} } } */
