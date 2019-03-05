/* { dg-do compile } */
/* { dg-options "-O2 -floop-interchange -fdump-tree-linterchange-details" } */
/* { dg-require-effective-target size20plus } */
/* { dg-skip-if "too big data segment" { visium-*-* } } */

#define M 256
int a[M][M], b[M][M];

void
simple_reduc_1 (int n, int *p)
{
  for (int j = 0; j < n; j++)
    {
      int sum = p[j];
      for (int i = 0; i < n; i++)
	{
	  sum = sum + b[i][j];
	  b[i][j] += a[i][j];
	}

      p[j] = sum;
    }
}
/* { dg-final { scan-tree-dump-not "Loop_pair<outer:., inner:.> is interchanged" "linterchange" } } */
