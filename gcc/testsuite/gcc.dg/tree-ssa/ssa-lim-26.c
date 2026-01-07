/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-lim2-details" } */

void foo (int n, int m, int *a, int *p, int * __restrict q)
{
  for (int i = 0; i < n; ++i)
    {
      int j = 0;
      do
        {
          q[j] = *a * p[i];
        }
      while (++j < m);
    }
}

/* Verify we can hoist *a two levels.  */ 
/* { dg-final { scan-tree-dump-times "out of loop 1" 1 "lim2" } } */
