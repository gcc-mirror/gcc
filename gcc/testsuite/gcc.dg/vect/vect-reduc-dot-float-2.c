/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target float16 } */
/* { dg-add-options float16 } */
/* { dg-additional-options "-O3 -ffast-math -ftree-vectorize -fdump-tree-vect-details" } */

float
dot_half_to_float (const _Float16 *restrict a,
		   const _Float16 *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

/* { dg-final { scan-tree-dump "vect_recog_dot_prod_pattern: detected" "vect" } } */
