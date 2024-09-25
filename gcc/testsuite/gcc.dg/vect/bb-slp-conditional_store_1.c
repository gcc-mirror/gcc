/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_float } */

/* { dg-additional-options "-mavx2" { target avx2 } } */
/* { dg-additional-options "-march=armv9-a" { target aarch64-*-* } } */

void foo3 (float *restrict a, int *restrict c)
{
#pragma GCC unroll 8
  for (int i = 0; i < 8; i++)
    c[i] = a[i] > 1.0;
}

/* { dg-final { scan-tree-dump "vectorized using SLP" "slp1" } } */
