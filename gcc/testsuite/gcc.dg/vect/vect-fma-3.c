/* PR tree-optimization/91723 */
/* { dg-do compile { target { scalar_all_fma || { i?86-*-* x86_64-*-* } } } } */
/* { dg-additional-options "-mfma" { target { i?86-*-* x86_64-*-* } } } */

void
foo (double *restrict r, const double *restrict a,
     const double *restrict b, const double *restrict c)
{
  for (int i = 0; i < 1024; i++)
    {
      double x = __builtin_fma (a[i], b[i], c[i]);
      x = __builtin_fma (a[i], b[i], x);
      r[i] = x;
    }
}

/* { dg-final { scan-tree-dump-times "LOOP VECTORIZED" 1 "vect" { target vect_double } } } */
