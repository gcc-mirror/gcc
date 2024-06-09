/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -march=rv64gcv_zvfh -mabi=lp64d -mrvv-vector-bits=scalable -fdump-tree-vect-details" } */

/* This test ensures that we vectorize the conversion by having the vectorizer
   create an intermediate type.  */

#include <stdint-gcc.h>

void convert (_Float16 *restrict dst, int64_t *restrict a, int n)
{
  for (int i = 0; i < n; i++)
    dst[i] = (_Float16) (a[i] & 0x7fffffff);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
