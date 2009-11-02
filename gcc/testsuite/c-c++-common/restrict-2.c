/* { dg-do compile } */
/* { dg-options "-O -fno-strict-aliasing -fdump-tree-lim-details" } */

void foo (float * __restrict__ a, float * __restrict__ b, int n, int j)
{
  int i;
  for(i = 0; i < n; ++i)
    a[i] = (b[j+50] + b[j-50]) * 0.5f;
}

/* We should move the RHS of the store out of the loop.  */

/* { dg-final { scan-tree-dump-times "Moving statement" 11 "lim1" } } */
/* { dg-final { cleanup-tree-dump "lim\[1-2\]" } } */
