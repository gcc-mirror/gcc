/* { dg-do compile } */
/* { dg-options "-O -fno-strict-aliasing -fdump-tree-lim1-details" } */

void f(int * __restrict__ r,
       int a[__restrict__ 16][16],
       int b[__restrict__ 16][16],
       int i, int j)
{
  int x;
  *r = 0;
  for (x = 1; x < 16; ++x)
    *r = *r + a[i][x] * b[x][j];
}

/* We should apply store motion to the store to *r.  */

/* { dg-final { scan-tree-dump "Executing store motion of \\\*r" "lim1" } } */
/* { dg-final { cleanup-tree-dump "lim1" } } */
