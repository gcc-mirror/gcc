void bar ();

int toto()
{
  int i,j, b;
  int a[100];

  for (j = 0; j <= 20; j++)
    {
      a[j] = b + i;
      
      if (j * i == b)
        break;

      a[j+b] = b + i;
    }

  a[i] = b + 3;


  return a[b];
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 0" 1 "graphite"} } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
