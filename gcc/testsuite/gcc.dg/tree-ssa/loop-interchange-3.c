/* { dg-do run } */
/* { dg-options "-O2 -floop-interchange -fdump-tree-linterchange-details" } */
/* { dg-require-effective-target size20plus } */
/* { dg-skip-if "too big stack" { visium-*-* } } */

/* Copied from graphite/interchange-6.c */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 100
#define M 200

static int __attribute__((noinline))
foo (int A[N][M])
{
  int i, j;

  /* This loop should be interchanged. */
  for(j = 0; j < M; j++)
    for(i = 0; i < N; i++)
      A[i][j] = A[i][j] + A[i][j];

  return A[0][0] + A[N-1][M-1];
}

extern void abort ();

static void __attribute__((noinline))
init (int *arr, int i)
{
  int j;

  for (j = 0; j < M; j++)
    arr[j] = 2;
}

int
main (void)
{
  int A[N][M];
  int i, j, res;

  for (i = 0; i < N; i++)
    init (A[i], i);

  res = foo (A);

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 8)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Loop_pair<outer:., inner:.> is interchanged" 1 "linterchange"} } */
