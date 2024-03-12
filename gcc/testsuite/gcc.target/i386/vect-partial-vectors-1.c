/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mavx512vl -mprefer-vector-width=512 --param vect-partial-vector-usage=1" } */

void foo (int * __restrict a, int *b)
{
  for (int i = 0; i < 4; ++i)
    a[i] = b[i] + 42;
}

/* We do not want to optimize this using masked AVX or AXV512
   but unmasked SSE.  */
/* { dg-final { scan-assembler-not "\[yz\]mm" } } */
/* { dg-final { scan-assembler "xmm" } } */
