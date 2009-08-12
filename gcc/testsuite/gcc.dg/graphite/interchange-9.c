int
foo (int *x)
{
  int i, j;
  int sum = 0;

  for (j = 0;  j < 10000; ++j)
    for (i = 0;  i < 10000; ++i)
      sum += x[10000 * i + j];
  return sum;
}

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" { xfail *-*-* } } } */ 
/* { dg-final { cleanup-tree-dump "graphite" } } */
