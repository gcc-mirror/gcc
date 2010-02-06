/* { dg-require-effective-target size32plus } */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 200

int a[N][N];
int b[N][N];

static int __attribute__((noinline))
foo (void)
{
  int i, j;
  int res = 0;

  /* This loop nest should be blocked.  */
  for (j = 1; j < N; j++)
    for (i = 0; i < N; i++)
      a[i][j] = a[i][j-1] + b[i][j];

  for (i = 0; i < N; i++)
    res += a[i][i];

  return res;
}

int
main (void)
{
  int i, j, res;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      {
	a[i][j] = i + j;
	b[i][j] = i - j;
      }

  res = foo ();

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  return res != 1333300;
}

/* { dg-final { scan-tree-dump-times "will be loop blocked" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
