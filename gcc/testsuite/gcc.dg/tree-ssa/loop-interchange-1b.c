/* { dg-do run } */
/* { dg-options "-O2 -floop-interchange -fdump-tree-linterchange-details" } */
/* { dg-require-effective-target size32plus } */
/* { dg-skip-if "too big data segment" { visium-*-* } } */

/* Copied from graphite/interchange-4.c */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

double u[1782225];

static void __attribute__((noinline))
foo (int N, double *res)
{
  int i, j;
  double sum = 0;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      sum = sum + u[i + 1335 * j];

  *res = sum;
}

extern void abort ();

int
main (void)
{
  int i, j;
  double res;

  for (i = 0; i < 1782225; i++)
    u[i] = 0;
  u[0] = __DBL_MAX__;
  u[1335] = -__DBL_MAX__;
  u[1] = __DBL_MAX__;
  u[1336] = -__DBL_MAX__;

  foo (1335, &res);

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 0.0)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "is interchanged" "linterchange"} } */
