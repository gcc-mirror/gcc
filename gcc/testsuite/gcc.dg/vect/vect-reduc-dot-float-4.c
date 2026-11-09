/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target float16 } */
/* { dg-add-options float16 } */
/* { dg-additional-options "-O3 -ffast-math -ftree-vectorize -fdump-tree-vect-details" } */

/* Multiplying before the conversion rounds each product to _Float16 and
   must not be represented as a widening dot product.  */
float
dot_rounded_half (const _Float16 *restrict a,
		  const _Float16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    {
      _Float16 product = a[i] * b[i];
      sum += (float) product;
    }
  return sum;
}

/* { dg-final { scan-tree-dump-not "vect_recog_dot_prod_pattern: detected" "vect" } } */
