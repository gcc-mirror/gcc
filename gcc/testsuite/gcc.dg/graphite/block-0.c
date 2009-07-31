#define N 1000

int toto()
{
  int j;
  int i;
  int a[N];
  int b[N];

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      a[j] = a[i] + 1;

  return a[0];
}

main()
{
  return toto();
}

/* { dg-final { scan-tree-dump-times "will be loop blocked" 1 "graphite" { xfail *-*-* } } } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
