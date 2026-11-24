/* PR tree-optimization/129007 */
/* { dg-additional-options "-ffast-math" } */

#include "tree-vect.h"

#define N 16

__attribute__((noipa)) void
f (_Complex double *restrict c, _Complex double *restrict x,
   _Complex double *restrict y, int n, int s)
{
  for (int i = 0; i < n; i++)
    c[i * s] -= x[i] * y[i];
}

int
main (void)
{
  _Complex double x[N], y[N], c[N];

  check_vect ();

  for (int i = 0; i < N; i++)
    {
      x[i] = (i + 1) + (i + 2) * 1.0i;
      y[i] = (i + 3) + (i + 4) * 1.0i;
      c[i] = 100 + 50 * 1.0i;
    }

  f (c, x, y, N, 1);

  for (int i = 0; i < N; i++)
    if (c[i] != 100 + 50 * 1.0i - x[i] * y[i])
      __builtin_abort ();
  return 0;
}
