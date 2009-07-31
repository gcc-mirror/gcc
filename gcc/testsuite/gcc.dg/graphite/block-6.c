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

/* Interchange is not legal for loops 0 and 1 of SCoP 2.  */
/* { dg-final { scan-tree-dump-times "Interchange not valid for loops 0 and 1:" 1 "graphite" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
