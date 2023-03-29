/* { dg-do compile } */
/* { dg-require-effective-target s390_vx } */
/* { dg-options "-O2 -mzarch -march=z13 -ftree-vectorize -fdump-tree-vect-details -fvect-cost-model=dynamic" } */

void foo(int *restrict a, int *restrict b, unsigned int n)
{
  for (unsigned int i = 0; i < n; i++)
    b[i] = a[i] * 2 + 1;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
/* { dg-final { scan-tree-dump "Vectorizing an unaligned access" "vect" } } */
