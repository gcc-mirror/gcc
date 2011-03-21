/* { dg-require-effective-target size32plus } */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

double u[1782225];

static void __attribute__((noinline))
foo (int N, int *res)
{
  int i, j;
  double sum = 0.0;

  /* These two loops should be interchanged.  */
  for (i = 0; i < 1335; i++)
    {
      for (j = 0; j < 1335; j++)
	sum = sum + u[i + 1335 * j];

      u[1336 * i] *= 2;
    }
  *res = sum;
}

extern void abort ();

int
main (void)
{
  int i, res;

  for (i = 0; i < 1782225; i++)
    u[i] = 2;

  foo (1335, &res);

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 3564450)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "will be interchanged" 1 "graphite" } } */
/* { dg-final { cleanup-tree-dump "graphite" } } */
