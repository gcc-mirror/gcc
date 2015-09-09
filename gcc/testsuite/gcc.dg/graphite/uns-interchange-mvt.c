/* { dg-require-effective-target size32plus } */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define NMAX 2000

static unsigned int x1[NMAX], x2[NMAX], a[NMAX][NMAX], y1[NMAX], y2[NMAX];

static unsigned int __attribute__((noinline))
mvt (long N)
{

  int i,j;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      x1[i] = x1[i] + a[i][j] * y1[j];

  /* These two loops should be interchanged.  */
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      x2[i] = x2[i] + a[j][i] * y2[j];

  return x1[0] + x2[0];
}

extern void abort ();

int
main (void)
{
  int i, j;
  unsigned int res;

  for (i = 0; i < NMAX; i++)
    for (j = 0; j < NMAX; j++)
      a[i][j] = i + j;

  for (i = 0; i < NMAX; i++)
    {
      x1[i] = 0;
      x2[i] = 2*i;
      y1[i] = 100 - i;
      y2[i] = i;
    }

  res = mvt (NMAX);

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 199900000)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "tiled by" "graphite" } } */
