/* PR tree-optimization/57741 */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-ffast-math" } */

#include "tree-vect.h"

extern void abort (void);

__attribute__((noinline, noclone)) void
foo (float *p, float *q, float x)
{
  int i;
  p = (float *) __builtin_assume_aligned (p, 32);
  q = (float *) __builtin_assume_aligned (q, 32);
  float f = 1.0f, g = 2.0f;
  for (i = 0; i < 1024; i++)
    {
      *p++ = f;
      f += x;
    }
  for (i = 0; i < 1024; i++)
    {
      *q++ = g;
      g += 0.5f;
    }
}

float p[1024] __attribute__((aligned (32))) = { 17.0f };
float q[1024] __attribute__((aligned (32))) = { 17.0f };

int
main ()
{
  int i;
  check_vect ();
  foo (p, q, 1.5f);
  for (i = 0; i < 1024; i++)
    if (p[i] != 1.0f + i * 1.5f || q[i] != 2.0f + i * 0.5f)
      abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 2 loop" 1 "vect" } } */
