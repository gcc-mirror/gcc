/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-O3 -fno-associative-math -ftree-vectorize -fdump-tree-vect-details" } */

double
dot_ordered (const float *restrict a, const float *restrict b, int n)
{
  double sum = 0.0;
  for (int i = 0; i < n; ++i)
    sum += (double) a[i] * (double) b[i];
  return sum;
}

/* Reassociation is required for a dot-product reduction.  */
/* { dg-final { scan-tree-dump-not "vect_recog_dot_prod_pattern: detected" "vect" } } */
