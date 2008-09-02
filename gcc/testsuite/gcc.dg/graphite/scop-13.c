/* { dg-options "-O2 -fgraphite -fdump-tree-graphite-all" } */

void bar ();

int toto()
{
  int i,j, b;
  int a[100];

  if (i == 20)
    {
      b = 3;
      goto B;
    }
  else 
    {
      if (i == 30)
	{
          a[i] = b;


          for (j = 0; j <= 20; j++)
            a[j] = b + i;

          B:

          for (j = 0; j <= 20; j++)
            a[j+b] = b + i;
          
          bar ();
	}
      else 
        {
          a[i] = b + 3;
        }
    }


  return a[b];
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 2" 1 "graphite"} } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
