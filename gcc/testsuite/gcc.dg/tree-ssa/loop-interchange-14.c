/* PR tree-optimization/83337 */
/* { dg-do run { target int32plus } } */
/* { dg-options "-O2 -floop-interchange -fdump-tree-linterchange-details" } */
/* { dg-skip-if "too big data segment" { visium-*-* } } */

/* Copied from graphite/interchange-5.c */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 100
#define M 1111
struct S { int a : 3; int b : 17; int c : 12; };
struct S A[N][M];

static int __attribute__((noinline))
foo (void)
{
  int i, j;

  for( i = 0; i < M; i++)
    for( j = 0; j < N; j++)
      A[j][i].b = 5 * A[j][i].b;

  return A[0][0].b + A[N-1][M-1].b;
}

extern void abort ();

static void __attribute__((noinline))
init (int i)
{
  int j;

  for (j = 0; j < M; j++)
    A[i][j].b = 2;
}

int
main (void)
{
  int i, j, res;

  for (i = 0; i < N; i++)
    init (i);

  res = foo ();

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 20)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "Loop_pair<outer:., inner:.> is interchanged" 1 "linterchange"} } */
