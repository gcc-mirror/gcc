/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp-simd -msve-vector-bits=256 -fno-vect-cost-model" } */

#include <stdint.h>

void
foo (uint64_t *restrict x, uint64_t *restrict y, uint64_t *restrict index)
{
#pragma omp for simd simdlen(2)
  for (int i = 0; i < 128; ++i)
    x[i] += y[index[i]];
}

/* { dg-final { scan-assembler-times {\tldr\td[0-9]+, \[x[0-9]+, x[0-9]+, lsl #?3\]} 2 } } */
/* { dg-final { scan-assembler-not {\tshl\tv[0-9]+\.2d,} } } */
/* { dg-final { scan-assembler-not {\tumov\t} } } */
/* { dg-final { scan-assembler {\tadd\tv[0-9]+\.2d,} } } */
/* { dg-final { scan-assembler {\tstr\tq[0-9]+,} } } */
