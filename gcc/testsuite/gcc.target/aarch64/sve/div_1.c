/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize -fno-vect-cost-model --save-temps" } */

#include <stdint.h>

#define DEF_LOOP(TYPE)					\
void __attribute__ ((noipa))				\
mod_##TYPE (TYPE *restrict dst, TYPE *restrict src1,	\
	    TYPE *restrict src2, int count)		\
{							\
  for (int i = 0; i < count; ++i)			\
    dst[i] = src1[i] / src2[i];				\
}

#define TEST_ALL(T) \
  T (int32_t) \
  T (uint32_t) \
  T (int64_t) \
  T (uint64_t)

TEST_ALL (DEF_LOOP)

/* { dg-final { scan-assembler-times {\tsdiv\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tudiv\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsdiv\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tudiv\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
