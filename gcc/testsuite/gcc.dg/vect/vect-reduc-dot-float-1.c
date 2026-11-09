/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-O3 -ffast-math -ftree-vectorize -fdump-tree-vect-details" } */

double
dot_float_to_double (const float *restrict a, const float *restrict b, int n)
{
  double sum = 0.0;
  for (int i = 0; i < n; ++i)
    sum += (double) a[i] * (double) b[i];
  return sum;
}

/* The pattern is recognized even on targets without a floating-point
   dot-product instruction.  */
/* { dg-final { scan-tree-dump "vect_recog_dot_prod_pattern: detected" "vect" } } */
