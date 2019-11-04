/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details --save-temps" } */

#include <stdint.h>

#ifndef OP
#define OP(x,y,z) (((x) & (z)) | ((y) & ~(z)))
#endif

#define TYPE(N) int##N##_t

#define TEMPLATE(SIZE)						\
void __attribute__ ((noinline, noclone))			\
f_##SIZE##_##OP							\
  (TYPE(SIZE) *restrict a, TYPE(SIZE) *restrict b,		\
   TYPE(SIZE) *restrict c, TYPE(SIZE) *restrict d, int n)	\
{								\
  for (int i = 0; i < n; i++)					\
    a[i] = OP (b[i], c[i], d[i]);				\
}

TEMPLATE (8);
TEMPLATE (16);
TEMPLATE (32);
TEMPLATE (64);

/* { dg-final { scan-tree-dump-times "vectorized 1 loops in function" 4 "vect" } } */

/* { dg-final { scan-assembler-not {\teor\tz[0-9]+\.[bhsd]} } } */
/* { dg-final { scan-assembler-not {\tand\tz[0-9]+\.[bhsd]} } } */

/* { dg-final { scan-assembler-times {\tbsl\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */
