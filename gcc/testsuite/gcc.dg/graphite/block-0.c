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

  /* This is not blocked as it is not profitable.  */
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      a[j] = a[i] + 1;

  return a[0];
}

extern void abort ();

int
main (void)
{
  int i, res;

  for (i = 0; i < N; i++)
    a[i] = i;

  res = foo ();

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 1999)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "will be loop blocked" "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
