/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-all -fno-vect-cost-model" } */

#define N 32

void
foo (double *output)
{
  int i = 0;
  /* Vectorizable.  */
  for (i = 0; i < N; i++)
    output[i] = 0.0;
}

/* { dg-final { scan-assembler "movi\\tv\[0-9\]+\\.2d, 0" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 1 "vect" } } */
