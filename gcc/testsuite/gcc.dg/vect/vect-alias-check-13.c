/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void
f1 (int *x, long step1, int n)
{
  for (int i = 0; i < n; ++i)
    x[i * step1] += 1;
}

void
f2 (int *x, long step2, int n)
{
#pragma GCC ivdep
  for (int i = 0; i < n; ++i)
    x[i * step2] += 2;
}

/* { dg-final { scan-tree-dump {need run-time check that [^\n]*step1[^\n]* is nonzero} "vect" } } */
/* { dg-final { scan-tree-dump-not {need run-time check that [^\n]*step2[^\n]* is nonzero} "vect" } } */
/* { dg-final { scan-tree-dump-not "using an address-based" "vect" } } */
/* { dg-final { scan-tree-dump-not "using an index-based" "vect" } } */
/* { dg-final { scan-tree-dump-times {LOOP VECTORIZED} 2 "vect" } } */
