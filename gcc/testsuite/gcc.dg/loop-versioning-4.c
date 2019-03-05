/* { dg-options "-O3 -fdump-tree-lversion-details" } */

/* These shouldn't be versioned; it's extremely likely that the code
   is emulating two-dimensional arrays.  */

void
f1 (double *x, int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[i * step + j] = 100;
}

void
f2 (double *x, int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[j * step + i] = 100;
}

void
f3 (double *x, int *offsets, int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[i * step + j + offsets[i]] = 100;
}

void
f4 (double *x, int *offsets, int step, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[j * step + i + offsets[i]] = 100;
}

/* { dg-final { scan-tree-dump-not {want to version} "lversion" } } */
/* { dg-final { scan-tree-dump-not {versioned} "lversion" } } */
