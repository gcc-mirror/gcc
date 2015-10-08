/* { dg-do run } */
/* { dg-options "-O2 -funswitch-loops" } */

#include <stdlib.h>
__attribute__ ((noinline))
void foo (float **a, float **b, float *c, int n, int m, int l)
{
  int i,j,k;
  float s;
  for (i=0; i<l; i++)
    for (j=0; j<n; j++)
      for (k=0; k<m; k++)
	c[i] += a[i][k] * b[k][j];
}

int main()
{
  const int N = 32;
  float **ar1, **ar2;
  float *res;
  int i, j;
  ar1 = (float **)malloc (N * sizeof (float*));
  ar2 = (float **)malloc (N * sizeof (float*));
  res = (float *)malloc( N * sizeof (float));
  for (i=0; i<N; i++)
    {
      ar1[i] = (float*)malloc (N * sizeof (float));
      ar2[i] = (float*)malloc (N * sizeof (float));
    }
  for (i=0; i<N; i++)
    {
      for (j=0; j<N; j++)
	{
	  ar1[i][j] = 2.0f;
	  ar2[i][j] = 1.5f;
	}
      res[i] = 0.0f;
    }
  foo (ar1, ar2, res, N, N, N);
  for (i=0; i<N; i++)
    if (res[i] != 3072.0f)
      abort();
  for (i=0; i<N; i++)
    res[i] = 0.0f;
  foo (ar1, ar2, res, N, 0, N);
  for (i=0; i<N; i++)
    if (res[i] != 0.0f)
      abort();
 
  return 0;
}

