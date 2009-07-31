#define N 10000
void foo (int);
int test ()
{
  int a[N][N];
  unsigned i, j;

  for (i = 0; i < N; i++) 
    for (j = 0; j < N; j++)
	a[i][j] = i*j;

  for (i = 1; i < N; i++) 
    for (j = 1; j < (N-1) ; j++)
	a[i][j] = a[i-1][j+1] * a[i-1][j+1]/2;

  for (i = 0; i < N; i++) 
    for (j = 0; j < N; j++)
      foo (a[i][j]); 
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 2" 1 "graphite"} } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
