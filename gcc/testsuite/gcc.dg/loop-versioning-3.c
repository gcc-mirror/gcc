/* { dg-options "-O3 -fdump-tree-lversion-details" } */

/* Versioning these loops for when both steps are 1 allows loop
   interchange, but otherwise isn't worthwhile.  At the moment we decide
   not to version.  */

void
f1 (double x[][100], int step1, int step2, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[j * step1][i * step2] = 100;
}

void
f2 (double x[][100], int step1, int step2, int limit)
{
  for (int i = 0; i < limit; i += step1)
    for (int j = 0; j < limit; j += step2)
      x[j][i] = 100;
}

/* { dg-final { scan-tree-dump-not {want to version} "lversion" } } */
/* { dg-final { scan-tree-dump-not {versioned} "lversion" } } */
