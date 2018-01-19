/* PR tree-optimization/83337 */
/* { dg-do run { target int32plus } } */
/* { dg-options "-O2 -floop-interchange" } */
/* { dg-require-effective-target alloca }  */
/* { dg-skip-if "too big stack" { visium-*-* } } */

/* Copied from graphite/interchange-5.c */

#define DEBUG 0
#if DEBUG
#include <stdio.h>
#endif

#define N 100
#define M 1111

extern void abort ();

static void __attribute__((noipa))
foo (int n)
{
  int i, j;
  struct S { char d[n]; int a : 3; int b : 17; int c : 12; };
  struct S A[N][M];

  for (i = 0; i < N; i++)
    {
      asm volatile ("" : : "g" (&A[0][0]) : "memory");
      for (j = 0; j < M; j++)
	A[i][j].b = 2;
    }
  asm volatile ("" : : "g" (&A[0][0]) : "memory");

  for (i = 0; i < M; i++)
    for (j = 0; j < N; j++)
      A[j][i].b = 5 * A[j][i].b;

  asm volatile ("" : : "g" (&A[0][0]) : "memory");
  int res = A[0][0].b + A[N-1][M-1].b;

#if DEBUG
  fprintf (stderr, "res = %d \n", res);
#endif

  if (res != 20)
    abort ();
}

int
main (void)
{
  foo (1);
  foo (8);
  return 0;
}
