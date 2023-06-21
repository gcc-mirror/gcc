/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define N 1024

#define TYPE(N) int##N##_t

#define TEMPLATE(SIZE)						\
void __attribute__ ((noinline, noclone))			\
f_##SIZE##_##OP							\
  (TYPE(SIZE) *restrict a, TYPE(SIZE) *restrict b,		\
   TYPE(SIZE) *restrict c)					\
{								\
  for (int i = 0; i < N; i++)					\
    a[i] = b[i] * c[i];						\
}

TEMPLATE (8);
TEMPLATE (16);
TEMPLATE (32);
TEMPLATE (64);

/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b} 1 } } */

