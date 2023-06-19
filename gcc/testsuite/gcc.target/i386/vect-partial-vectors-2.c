/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mavx512vl -mprefer-vector-width=512 --param vect-partial-vector-usage=1" } */

void foo (int * __restrict a, int *b)
{
  for (int i = 0; i < 7; ++i)
    a[i] = b[i] + 42;
}

/* We want to optimize this using masked AVX, not AXV512 or SSE.  */
/* { dg-final { scan-assembler-not "zmm" } } */
/* { dg-final { scan-assembler "ymm\[^\r\n\]*\{%k" } } */
