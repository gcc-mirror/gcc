/* { dg-require-effective-target size32plus } */

#define N 10000
void foo (int);
int test ()
{
  int a[N][N];
  int b[N][N];
  unsigned i, j;

  for (i = 0; i < N; i++) 
    for (j = 0; j < N; j++)
      a[i][j] = i*j;

  for (j = 1; j < N; j++) 
    for (i = 0; i < N; i++)
      a[i][j] = a[i][j-1] + b[i][j];

  for (i = 0; i < N; i++) 
    for (j = 0; j < N; j++) 
      foo (a[i][j]); 
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 2" 1 "graphite"} } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
