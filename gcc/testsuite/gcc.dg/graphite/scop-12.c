void bar ();

int toto()
{
  int i,j, b;
  int a[100];

  switch (i)
    {
    
      case 5:
        for (j = 0; j <= 20; j++)
          a[j] = b + i + 12;
        break;
      case 8:
        for (j = 0; j <= 20; j++)
          a[j] = b + i + 122;
        break;
      case 15:
        for (j = 0; j <= 20; j++)
          a[j] = b + i + 12;
        break;
      case 18:
        for (j = 0; j <= 20; j++)
          a[j] = b + i + 4;
        break;
     default:
        for (j = 0; j <= 20; j++)
          a[j] = b + i + 3;
   }

  return a[b];
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 0" 1 "graphite"} } */
