/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-O3 -ffast-math -fdump-tree-vect-details" } */

float
dot_int_to_float (const short *restrict a, const short *restrict b, int n)
{
  float sum = 0.0f;
  for (int i = 0; i < n; ++i)
    sum += (float) a[i] * (float) b[i];
  return sum;
}

/* Integer-to-float conversions do not match a floating-point dot product.
   The operands must be widened from a narrower floating-point type.  */
/* { dg-final { scan-tree-dump-not "vect_recog_dot_prod_pattern: detected" "vect" } } */
