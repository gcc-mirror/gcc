/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -mrvv-max-lmul=dynamic -mrvv-vector-bits=zvl -fdump-tree-vect-details" } */

#define TYPE double
#define N 200

#include <complex.h>

void addconjboth (_Complex TYPE a[restrict N], _Complex TYPE b[restrict N],
		  _Complex TYPE c[restrict N])
{
#if defined (UNROLL)
#pragma GCC unroll 16
#endif
  for (int i=0; i < N; i++)
    c[i] = ~a[i] + ~b[i];
}

/* { dg-final { scan-assembler {e64,m4} } } */
/* { dg-final { scan-tree-dump-times "Maximum lmul = 4, At most 16 number of live V_REG" 1 "vect" } } */
