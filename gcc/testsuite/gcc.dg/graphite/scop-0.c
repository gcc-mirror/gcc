int foo (void);
void bar (void);

int toto()
{
  /* Scop 1. */
  int i, j, k;
  int a[100][100];
  int b[100];
  int N = foo ();

  for (i = 0; i < 2*N+ 100; i++)
    for (j = 0; j < 200; j++)
      a[j][i] = a[j+1][10] + 2;

  return a[3][5] + b[1];
  /* End scop 1. */
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 1" 1 "graphite"} } */
/* { dg-final { cleanup-tree-dump "graphite" } } */

