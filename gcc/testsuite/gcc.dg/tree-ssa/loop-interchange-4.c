/* { dg-do run } */
/* { dg-options "-O2 -floop-interchange -fdump-tree-linterchange-details" } */
/* { dg-require-stack-size "8*111*1111" } */
/* { dg-skip-if "too big stack" { avr-*-* visium-*-* } } */

/* Copied from graphite/interchange-7.c */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 111
#define M 1111

static int __attribute__((noinline))
foo (double *a)
{
  int i,j;
  int r = 0;

  for (i = 0; i < N; ++i)
    for (j = 0; j < M; ++j)
      r += a[j * N + i];

  return r;
}

extern void abort ();

int
main (void)
{
  double A[N*M];
  int i, res;

  for (i = 0; i < N*M; i++)
    A[i] = 2;

  res = foo (A);

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 246642)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Loop_pair<outer:., inner:.> is interchanged" 1 "linterchange" { xfail *-*-* } } } */
