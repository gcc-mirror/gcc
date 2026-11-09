/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-O3 -ffast-math -fdump-tree-vect-details" } */

double
dot_rounded_float (const float *restrict a, const float *restrict b, int n)
{
  double sum = 0.0;
  for (int i = 0; i < n; ++i)
    {
      float product = a[i] * b[i];
      sum += (double) product;
    }
  return sum;
}

/* Rounding the product to float before widening is not a widening dot
   product, even when the summation can be reassociated.  */
/* { dg-final { scan-tree-dump-not "vect_recog_dot_prod_pattern: detected" "vect" } } */
