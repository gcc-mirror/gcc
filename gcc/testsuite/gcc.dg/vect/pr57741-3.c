/* PR tree-optimization/57741 */
/* { dg-do run } */
/* { dg-additional-options "-ffast-math" } */

#include "tree-vect.h"

extern void abort (void);

float p[1024] __attribute__((aligned (32))) = { 17.0f };
float q[1024] __attribute__((aligned (32))) = { 17.0f };
char r[1024] __attribute__((aligned (32))) = { 1 };

__attribute__((noinline, noclone)) void
foo (float x)
{
  int i;
  float f = 1.0f, g = 2.0f;
  for (i = 0; i < 1024; i++)
    {
      p[i] = f;
      f += x;
      q[i] = g;
      g += 0.5f;
      r[i]++;
    }
}

int
main ()
{
  int i;
  check_vect ();
  r[0] = 0;
  foo (1.5f);
  for (i = 0; i < 1024; i++)
    if (p[i] != 1.0f + i * 1.5f || q[i] != 2.0f + i * 0.5f || r[i] != 1)
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */
