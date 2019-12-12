/* { dg-options "-O3 -fdump-tree-lversion-details" } */

/* Test that we don't try to version for something that is never 1.  */

void
f1 (double *x, int stepx, int n)
{
  if (stepx == 1)
    for (int i = 0; i < n; ++i)
      x[i] = 100;
  else
    for (int i = 0; i < n; ++i)
      x[stepx * i] = 100;
}

void
f2 (double *x, int stepx, int n)
{
  if (stepx <= 1)
    for (int i = 0; i < n; ++i)
      x[i] = 100;
  else
    for (int i = 0; i < n; ++i)
      x[stepx * i] = 100;
}

/* { dg-final { scan-tree-dump-times {want to version containing loop} 2 "lversion" } } */
/* { dg-final { scan-tree-dump-times {can never be 1} 2 "lversion" } } */
/* { dg-final { scan-tree-dump-not {versioned} "lversion" } } */
