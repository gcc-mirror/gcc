/* { dg-options "-O3 -fdump-tree-lversion-details" } */

/* There's no information about whether STEP1 or STEP2 is innermost,
   so we should assume the code is sensible and version for the inner
   evolution, i.e. when STEP2 is 1.  */

void
f1 (double *x, int step1, int step2, int n)
{
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      x[i * step1 + j * step2] = 100;
}

/* { dg-final { scan-tree-dump-times {want to version containing loop for when step2} 1 "lversion" } } */
/* { dg-final { scan-tree-dump-times {want to version containing loop} 1 "lversion" } } */
/* { dg-final { scan-tree-dump-times {versioned this loop} 1 "lversion" } } */
