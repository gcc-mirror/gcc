/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -fdump-tree-vect-details" } */

#include <stdint-gcc.h>

void
foo (int8_t *__restrict a, int8_t *__restrict b, int n)
{
  for (int i = 0; i < n; i++){
    a[i] = a[i] + b[i];
    a[i] = a[i] + b[i];
    a[i] = a[i] + b[i];
    a[i] = a[i] + b[i];
    a[i] = a[i] + b[i];
    a[i] = a[i] + b[i];
    a[i] = a[i] + b[i];
  }
}

/* { dg-final { scan-assembler {e8,m8} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-tree-dump-not "Preferring smaller LMUL loop because it has unexpected spills" "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 8" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 4" 1 "vect" } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 2" 1 "vect" } } */
