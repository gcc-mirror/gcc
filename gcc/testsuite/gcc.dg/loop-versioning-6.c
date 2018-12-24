/* { dg-options "-O3 -fdump-tree-lversion-details" } */

/* The read from y in f1 will be hoisted to the outer loop.  In general
   it's not worth versioning outer loops when the inner loops don't also
   benefit.

   This test is meant to be a slight counterexample, since versioning
   does lead to cheaper outer-loop vectorization.  However, the benefit
   isn't enough to justify the cost.  */

void
f1 (double *restrict x, double *restrict y, int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[i + j] = y[i * step];
}

/* A similar example in which the read can't be hoisted, but could
   for example be handled by vectorizer alias checks.  */

void
f2 (double *x, double *y, int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[i + j] = y[i * step];
}

/* { dg-final { scan-tree-dump-not {want to version} "lversion" } } */
/* { dg-final { scan-tree-dump-not {versioned} "lversion" } } */
