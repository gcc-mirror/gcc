void bar (void);

int toto()
{
  int i, j, k;
  int a[100][100];
  int b[200];

  for (i = 1; i < 100; i++)
    {
      for (j = 1; j < 100; j++)
        b[i+j] = b[i+j-1] + 2;

      if (i * 2 == i + 8)
	{
	  for (j = 1; j < 100; j++)
	    b[i+j] = b[i+j-1] + 2;
	}
      else 
	a[i][i] = 2;

      for (k = 1; k < 100; k++)
        b[i+k] = b[i+k-5] + 2;
    }

  return a[3][5] + b[2];
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 1" 1 "graphite"} } */
