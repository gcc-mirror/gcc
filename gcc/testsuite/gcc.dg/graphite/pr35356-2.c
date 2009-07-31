/* { dg-options "-O2 -fgraphite-identity -fdump-tree-graphite-all" } */

int a[100];

int
foo (int bar, int n, int k)
{
  int i;

  for (i = 0; i < n; i++)
    if (i == k)
      a[i] = 1;
    else 
      a[i] = i;

  return a[bar];
}

/* We should generate the following:

   | for (i = 0; i < min (n, k); i++)
   |   a[i] = i;
   | if (k >= 0 && k < n)
   |   a[k] = 1;
   | for (i = max(k+1,0); i < n; i++)
   |   a[i] = i;

*/

/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "graphite" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
