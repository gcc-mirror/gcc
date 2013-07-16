int toto()
{
  int i, j, k;
  int a[100][200];
  int b[100];

  for (i = 1; i < 100; i++)
    {
      for (j = 1; j < 80; j++)
	a[j][i] = a[j+1][2*i-1*j] + 12;

      b[i] = b[i-1] + 10;

      for (j = 1; j < 60; j++)
	a[j][i] = a[j+1][i-1] + 8;

      if (i == 23)
	b[i] = a[i-1][i] + 6;

      for (j = 1; j < 40; j++)
	a[j][i] = a[j+1][i-1] + 4;
    }

  return a[3][5] + b[1];
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 1" 1 "graphite"} } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
