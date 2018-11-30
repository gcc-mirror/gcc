/* { dg-do run } */
/* { dg-options "-O2 -floop-interchange -fassociative-math -fno-signed-zeros -fno-trapping-math -fdump-tree-linterchange-details" } */
/* { dg-require-effective-target size32plus } */
/* { dg-skip-if "too big data segment" { visium-*-* } } */

/* Copied from graphite/interchange-4.c */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

double u[1782225];

static int __attribute__((noinline))
foo (int N, int *res)
{
  int i, j;
  double sum = 0;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      sum = sum + u[i + 1335 * j];

  for (i = 0; i < N; i++)
    u[1336 * i] *= 2;

  *res = sum + N + u[1336 * 2] + u[1336];
}

extern void abort ();

int
main (void)
{
  int i, j, res;

  for (i = 0; i < 1782225; i++)
    u[i] = 2;

  foo (1335, &res);

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 3565793)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Loop_pair<outer:., inner:.> is interchanged" 1 "linterchange"} } */
