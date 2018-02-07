/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized-raw -fno-tree-slp-vectorize" } */

void
cse_recip (float x, float y, float *a)
{
  a[0] = y / (5 * x);
  a[1] = y / (3 * x);
  a[2] = y / x;
}

/* { dg-final { scan-tree-dump-times "rdiv_expr" 1 "optimized" } } */
