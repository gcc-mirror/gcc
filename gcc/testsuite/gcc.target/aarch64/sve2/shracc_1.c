/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details --save-temps" } */

#include <stdint.h>

#define SHRACC(TYPE,SHIFT)			\
void __attribute__ ((noinline, noclone))	\
f_##TYPE##_##SHIFT				\
  (TYPE *restrict a, TYPE *restrict b, int n)	\
{						\
  for (int i = 0; i < n; i++)			\
    a[i] += b[i] >> (SHIFT);			\
}

SHRACC (int8_t, 5);
SHRACC (int16_t, 14);
SHRACC (int32_t, 19);
SHRACC (int64_t, 27);

SHRACC (uint8_t, 2);
SHRACC (uint16_t, 6);
SHRACC (uint32_t, 24);
SHRACC (uint64_t, 53);

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 8 "vect" } } */

/* { dg-final { scan-assembler-not {\tasr\tz[0-9]+\.[bhsd]} } } */
/* { dg-final { scan-assembler-not {\tlsr\tz[0-9]+\.[bhsd]} } } */
/* { dg-final { scan-assembler-not {\tadd\tz[0-9]+\.[bhsd]} } } */

/* { dg-final { scan-assembler-times {\tssra\tz[0-9]+\.b, z[0-9]+\.b, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tssra\tz[0-9]+\.h, z[0-9]+\.h, #14\n} 1 } } */
/* { dg-final { scan-assembler-times {\tssra\tz[0-9]+\.s, z[0-9]+\.s, #19\n} 1 } } */
/* { dg-final { scan-assembler-times {\tssra\tz[0-9]+\.d, z[0-9]+\.d, #27\n} 1 } } */

/* { dg-final { scan-assembler-times {\tusra\tz[0-9]+\.b, z[0-9]+\.b, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tusra\tz[0-9]+\.h, z[0-9]+\.h, #6\n} 1 } } */
/* { dg-final { scan-assembler-times {\tusra\tz[0-9]+\.s, z[0-9]+\.s, #24\n} 1 } } */
/* { dg-final { scan-assembler-times {\tusra\tz[0-9]+\.d, z[0-9]+\.d, #53\n} 1 } } */
