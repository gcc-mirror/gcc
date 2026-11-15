/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim2-details" } */

void
foo (int *p, int *q, int n, int m)
{
  for (int i = 0; i < n; ++i)
    {
      int j = 0;
      do
	p[j] = *q + i;
      while (++j < m);
    }
}

/* Reuse the cached result for the inner loop.  Each loop should require
   only one RAW dependence query.  */
/* { dg-final { scan-tree-dump-times "Querying RAW dependencies" 2 "lim2" } } */
