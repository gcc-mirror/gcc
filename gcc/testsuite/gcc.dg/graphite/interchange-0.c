/* { dg-require-effective-target size32plus } */

#define DEBUG 0

#if DEBUG
#include <stdio.h>
#endif

#define N 1000
int a[N][N];

static int __attribute__((noinline))
foo (void)
{
  int j;
  int i;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      a[j][i] = a[j][i] + 1;

  return a[N-1][N-1];
}

extern void abort ();

int
main (void)
{
  int i, j, res;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      a[i][j] = 1;

  a[N-1][N-1] = 12;
  res = foo ();

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 13)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
