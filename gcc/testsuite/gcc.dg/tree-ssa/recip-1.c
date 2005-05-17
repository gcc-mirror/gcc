/* { dg-do compile } */
/* { dg-options "-O1 -funsafe-math-optimizations -fdump-tree-recip" } */

float e(float *x, float *y, float *z)
{
  float m = __builtin_sqrt (*x * *x + *y * *y + *z * *z);
  *x /= m;
  *y /= m;
  *z /= m;
}

/* Look for only one division.  */
/* { dg-final { scan-tree-dump-times "= .* /" 1 "recip" } } */
/* { dg-final { cleanup-tree-dump "recip" } } */
