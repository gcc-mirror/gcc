/* { dg-options "-O2 -fgraphite-identity -fdump-tree-graphite-all" } */

int a[100];

int
foo (int bar, int n, int k)
{
  int i;

  for (i = 0; i < n; i++)
    if (i == k)
      a[i] = bar;

  return a[bar];
}

/* There should be no loops generated for this testcase, instead we
   should generate the following:

   | if (k >= 0 && k < n)
   |   a[k] = bar;

*/

/* { dg-final { scan-tree-dump-times "loop_1" 0 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
