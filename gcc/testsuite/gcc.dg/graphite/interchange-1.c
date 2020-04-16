/* { dg-require-effective-target size32plus } */

/* Formerly known as ltrans-1.c */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

double u[1782225];

static int __attribute__((noinline))
foo (int N)
{
  int i, j;
  double sum = 0.0;

  /* These two loops should be interchanged.  */
  for (i = 0; i < N; i++)
    {
      for (j = 0; j < N; j++)
	sum = sum + u[i + 1335 * j];

      u[1336 * i] *= 2;
    }

  return sum + N + u[1336 * 2] + u[1336];
}

extern void abort ();

int
main (void)
{
  int i, j, res;

  for (i = 0; i < 1782225; i++)
    u[i] = 2;

  res = foo (1335);

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 3565793)
    abort ();

  return 0;
}
