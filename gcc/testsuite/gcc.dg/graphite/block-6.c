/* { dg-require-effective-target size32plus } */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 200
int a[N][N];

static int __attribute__((noinline))
foo (void)
{
  int i, j;
  int res = 0;

  /* Interchange is not legal for loops 0 and 1.  */
  for (i = 1; i < N; i++)
    for (j = 1; j < N - 1; j++)
      a[i][j] = a[i-1][j+1] * a[i-1][j+1] / 2;

  for (i = 0; i < N; i++)
    res += a[i][i];

  return res;
}

extern void abort ();

int
main (void)
{
  int i, j, res;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      a[i][j] = i + j;

  res = foo ();

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 204007516)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "tiled by" "graphite" } } */
