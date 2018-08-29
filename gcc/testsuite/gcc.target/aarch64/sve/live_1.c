/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#include <stdint.h>

#define EXTRACT_LAST(TYPE)			\
  TYPE __attribute__ ((noinline, noclone))	\
  test_##TYPE (TYPE *x, int n, TYPE value)	\
  {						\
    TYPE last;					\
    for (int j = 0; j < n; ++j)			\
      {						\
	last = x[j];				\
	x[j] = last * value;			\
      }						\
    return last;				\
  }

#define TEST_ALL(T)				\
  T (uint8_t)					\
  T (uint16_t)					\
  T (uint32_t)					\
  T (uint64_t)					\
  T (_Float16)					\
  T (float)					\
  T (double)

TEST_ALL (EXTRACT_LAST)

/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7].b, } 2 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7].h, } 4 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7].s, } 4 } } */
/* { dg-final { scan-assembler-times {\twhilelo\tp[0-7].d, } 4 } } */

/* { dg-final { scan-assembler-times {\tlastb\tw[0-9]+, p[0-7], z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\tw[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\tw[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\tx[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\th[0-9]+, p[0-7], z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\ts[0-9]+, p[0-7], z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlastb\td[0-9]+, p[0-7], z[0-9]+\.d\n} 1 } } */
