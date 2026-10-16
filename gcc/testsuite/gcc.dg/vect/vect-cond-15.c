/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_condition } */
/* { dg-require-effective-target vect_intfloat_cvt } */
/* { dg-additional-options "-fdump-tree-optimized" } */

/* The negated comparison becomes a { 0, 1 } integer select; it is done
   on the float constants instead of being converted.  */

void
f (float *__restrict r, float *__restrict a, float *__restrict b, int n)
{
  for (int i = 0; i < n; i++)
    r[i] = !(a[i] >= b[i]);
}

void
g (float *__restrict r, float *__restrict a, float *__restrict b, int n)
{
  for (int i = 0; i < n; i++)
    r[i] = !(a[i] < b[i]);
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 2 "vect" } } */
/* { dg-final { scan-tree-dump-not "= \\(vector\\(\[^)\]*\\) float\\)" "optimized" } } */
