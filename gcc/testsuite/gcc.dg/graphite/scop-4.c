void bar ();

int toto()
{
  int i, j, k;
  int a[100][100];
  int b[100];

  for (i = 1; i < 100; i++)
    {
      for (j = 1; j < 80; j++)
	a[j][i] = a[j+1][2*i-1*j] + 12;

      b[i] = b[i-1] + 10;

      for (j = 1; j < 60; j++)
	a[j][i] = a[j+1][i-1] + 8;

      bar ();

      if (i == 23)
	b[i] = a[i-1][i] + 6;
    }

  return a[3][5] + b[1];
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 1" 1 "graphite"} } */
