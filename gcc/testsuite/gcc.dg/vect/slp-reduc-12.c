/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_hw_misalign } */
/* { dg-additional-options "-Ofast" } */

double foo (double *x, int * __restrict a, int n)
{
  double r = 0.;
  for (int i = 0; i < n; ++i)
    {
      a[i] = a[i] + i;
      r += x[i];
    }
  return r;
}

/* { dg-final { scan-tree-dump "using single def-use cycle for reduction" "vect" } } */
