double u[1782225];
int foo(int N, int *res)
{
  int i, j;
  double sum = 0.0;

  for (i = 0; i < 1335; i++)
    {
      for (j = 0; j < 1335; j++)
	sum = sum + u[i + 1335 * j];

      u[1336 * i] *= 2;
    }
  *res = sum;
}

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
