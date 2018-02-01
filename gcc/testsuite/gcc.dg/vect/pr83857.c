/* { dg-additional-options "-ffast-math" } */

#define N 100

double __attribute__ ((noinline, noclone))
f (double *x, double y)
{
  double a = 0;
  for (int i = 0; i < N; ++i)
    {
      a += y;
      x[i * 2] += a;
      x[i * 2 + 1] += a;
    }
  return a - y;
}

double x[N * 2];

int
main (void)
{
  if (f (x, 5) != (N - 1) * 5)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" "vect" { target vect_double } } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target vect_double } } } */
