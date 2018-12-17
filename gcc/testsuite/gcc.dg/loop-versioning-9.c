/* { dg-options "-O3 -fdump-tree-lversion-details" } */

/* Check that versioning can handle small groups of accesses.  */

void
f1 (int *x, int *y, int step, int n)
{
  for (int i = 0; i < n; ++i)
    x[i] = y[i * step * 2] + y[i * step * 2 + 1];
}

void
f2 (int *x, int *y, __INTPTR_TYPE__ step, int n)
{
  for (int i = 0; i < n; ++i)
    x[i] = y[i * step * 2] + y[i * step * 2 + 1];
}

void
f3 (int *x, int *y, int step, int n)
{
  for (int i = 0; i < n; ++i)
    x[i] = y[i * step * 3] + y[i * step * 3 + 2];
}

void
f4 (int *x, int *y, __INTPTR_TYPE__ step, int n)
{
  for (int i = 0; i < n; ++i)
    x[i] = y[i * step * 3] + y[i * step * 3 + 2];
}

void
f5 (int *x, int *y, int step, int n)
{
  for (int i = 0; i < n; ++i)
    x[i] = y[i * step * 4] + y[i * step * 4 + 3];
}

void
f6 (int *x, int *y, __INTPTR_TYPE__ step, int n)
{
  for (int i = 0; i < n; ++i)
    x[i] = y[i * step * 4] + y[i * step * 4 + 3];
}

/* { dg-final { scan-tree-dump-times {want to version containing loop} 6 "lversion" } } */
/* { dg-final { scan-tree-dump-times {versioned this loop} 6 "lversion" } } */
