/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mtune=generic-ooo -O3 -ftree-vectorize -fdump-tree-vect-details" } */

#include <stdint-gcc.h>

void
f2 (uint64_t *__restrict y, uint64_t *__restrict x,
    uint64_t *__restrict indices, uint64_t n)
{
  for (int64_t i = 0; i < n; ++i)
    {
      y[i * 2] = x[indices[i * 2]] + 1;
      y[i * 2 + 1] = x[indices[i * 2 + 1]] + 2;
    }
}

/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" vect } } */
/* { dg-final { scan-assembler-not "vlseg" } } */
/* { dg-final { scan-assembler-not "vsseg" } } */
