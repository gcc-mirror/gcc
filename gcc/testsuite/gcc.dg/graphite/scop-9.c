/* { dg-options "-O2 -fgraphite -fdump-tree-graphite-all" } */

void bar (void);

int toto()
{
  int i, j, k;
  int a[100][100];
  int b[100];

  for (i = 1; i < 100; i++)
    {
      for (j = 1; j < 100; j++)
        b[i+j] = b[i+j-1] + 2;

      if (i * 2 == i + 8)
        bar ();
      else 
	a[i][i] = 2;

      for (k = 1; k < 100; k++)
        b[i+k] = b[i+k-5] + 2;
    }

  return a[3][5] + b[1];
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 2" 1 "graphite"} } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
