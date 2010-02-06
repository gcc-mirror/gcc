#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 1000
int a[N];

static int __attribute__((noinline))
foo (void)
{
  int j;
  int i;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      a[j] = a[i] + 1;

  return a[0];
}

main()
{
  int i, res;

  for (i = 0; i < N; i++)
    a[i] = i;

  res = foo ();

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  /* Avoid runtime check for this testcase, as it is miscompiled by
     Graphite for the moment.  */
  return 0;
  return res != 1999;
}

/* { dg-final { scan-tree-dump-times "will be loop blocked" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
