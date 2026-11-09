/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target float16 } */
/* { dg-add-options float16 } */
/* { dg-additional-options "-O3 -ffast-math -fdump-tree-vect-details" } */

/* The accumulator can be more than twice as wide as the inputs.  */
double
dot_half_to_double (const _Float16 *restrict a,
		    const _Float16 *restrict b, int n)
{
  double sum = 0.0;
  for (int i = 0; i < n; ++i)
    sum += (double) a[i] * (double) b[i];
  return sum;
}

/* { dg-final { scan-tree-dump "vect_recog_dot_prod_pattern: detected" "vect" } } */
