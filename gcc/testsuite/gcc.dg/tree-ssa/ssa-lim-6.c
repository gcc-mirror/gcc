/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim1-details" } */

double a[16][64], y[64], x[16];
void foo(void)
{
  int i, j;
  for (j = 0; j < 64; ++j)
    for (i = 0; i < 16; ++i)
      y[j] = y[j] + a[i][j] * x[i];
}

/* { dg-final { scan-tree-dump "Executing store motion of y" "lim1" } } */
/* { dg-final { cleanup-tree-dump "lim1" } } */
