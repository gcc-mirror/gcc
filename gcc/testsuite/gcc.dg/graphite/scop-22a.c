/* { dg-require-effective-target size32plus } */
double u[1782225];

void foo(int N, int *res)
{
  int i, j;
  double a, b;
  double sum = 0.0;

  for (j = 3; j < N; j = j * j)
    {
      sum += a + b;
    }

  /* Next two loops form first SCoP */
  for (i = 0; i < N; i++)
    sum += u[i];

  for (i = 0; i < N; i++)
    {
      a = u[i];
      u[i] = i * i;
      b = u[i];
      sum += a + b;
    }

  for (j = 3; j < N; j = j * j)
    {
      sum += a + b;
    }

  for (j = 3; j < N; j = j * j)
    {
      sum += a + b;
    }

  /* Next two loop-nests form second SCoP */
  for (i = 0; i < N; i++)
    sum += u[i];

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      {
	a = u[i];
	u[i] = i * i;
	b = u[j];
	sum += a + b;
      }

  *res = sum + N;
}

/* { dg-final { scan-tree-dump-times "number of SCoPs: 2" 1 "graphite"} } */
/* { dg-final { scan-tree-dump-times "Loops in SCoP" 2 "graphite"} } */
/* { dg-final { scan-tree-dump "Loops in SCoP: 2, 3" "graphite"} } */
/* { dg-final { scan-tree-dump "Loops in SCoP: 6, 7, 8" "graphite"} } */
