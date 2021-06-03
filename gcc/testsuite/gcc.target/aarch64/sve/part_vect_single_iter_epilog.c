/* { dg-do compile } */
/* { dg-options "-O3 --param vect-partial-vector-usage=1" } */

void
foo (short * __restrict__ a, short * __restrict__ b, short * __restrict__ c, int n)
{
  for (int i = 0; i < n; ++i)
    c[i] = a[i] + b[i];
}

/* { dg-final { scan-assembler-times {\twhilelo\tp[0-9]+.h, wzr, [xw][0-9]+} 1 } } */
