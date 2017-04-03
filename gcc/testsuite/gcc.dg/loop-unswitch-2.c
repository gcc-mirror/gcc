/* { dg-do compile } */
/* { dg-options "-O2 -funswitch-loops -fdump-tree-unswitch-details" } */

void foo (float **a, float **b, float *c, int n, int m, int l)
{
  int i,j,k;
  float s;
  for (i=0; i<l; i++)
    for (j=0; j<n; j++)
      for (k=0; k<m; k++)
	c[i] += a[i][k] * b[k][j];
}

/* { dg-final { scan-tree-dump-times "guard hoisted" 3 "unswitch" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "unswitch" } } */

