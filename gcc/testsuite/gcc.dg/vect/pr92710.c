/* { dg-do compile } */
/* { dg-additional-options "-fopenmp-simd" } */

#pragma omp declare simd
_Bool foo (_Bool) __attribute__((const));

void
f (_Bool *restrict x, char *restrict y, char *restrict z)
{
  for (int i = 0; i < 128; ++i)
    x[i] = foo (y[i] == z[i]);
}
