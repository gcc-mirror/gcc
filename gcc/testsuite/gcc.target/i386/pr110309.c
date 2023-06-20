/* { dg-do compile } */
/* { dg-options "-O3 --param vect-partial-vector-usage=1 -march=znver4 -mprefer-vector-width=256" } */
/* { dg-final { scan-assembler-not {(?n)vpblendd.*ymm} } } */


void foo (int * __restrict a, int *b)
{
  for (int i = 0; i < 6; ++i)
    a[i] = b[i] + 42;
}
