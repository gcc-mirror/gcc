/* { dg-do run } */
/* { dg-options "-O2 -floop-interchange -fdump-tree-linterchange-details" } */

/* Copied from graphite/interchange-4.c */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

unsigned u[1024];

static void __attribute__((noinline,noclone,noipa))
foo (int N, unsigned *res)
{
  int i, j;
  unsigned sum = 1;
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      sum = u[i + 2 * j] / sum;

  *res = sum;
}

extern void abort ();

int
main (void)
{
  int i, j;
  unsigned res;

  u[0] = 10;
  u[1] = 200;
  u[2] = 10;
  u[3] = 10;

  foo (2, &res);

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 0)
    abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "is interchanged" "linterchange"} } */
