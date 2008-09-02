/* { dg-options "-O2 -fgraphite -fdump-tree-graphite-all" } */

void bar ();

int toto()
{
  int i,j, b;
  int a[100];

  if (i == 20)
    {
      for (j = 0; j <= 20; j++)
        a[j] = b + i;
      b = 3;
      bar();
    }
  else 
    {
      if (i == 30)
	{
          for (j = 0; j <= 20; j++)
            a[j] = b + i;
	  b = 5;
	}
    }

  for (j = 0; j <= 20; j++)
    a[j] = b + i;

  return a[b];
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 3" 1 "graphite"} } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
