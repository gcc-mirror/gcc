int a[1000][1000];

int
foo (int N)
{
  int j;
  int i;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      a[j][i] = a[j][i] + 1;

  return a[N][123];
}

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" } } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
