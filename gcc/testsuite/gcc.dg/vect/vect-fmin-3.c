/* { dg-require-effective-target vect_float } */

#include "tree-vect.h"

void __attribute__((noipa))
test (double x0, double x1, double *restrict res, double *restrict ptr, int n)
{
  for (int i = 0; i < n; i += 2)
    {
      x0 = __builtin_fmin (x0, ptr[i + 0]);
      x1 = __builtin_fmin (x1, ptr[i + 1]);
    }
  res[0] = x0;
  res[1] = x1;
}

#define N 128
#define HALF (N / 2)

int
main (void)
{
  check_vect ();

  double res[2], a[N];

  for (int i = 0; i < N; i += 2)
    {
      a[i] = i < HALF ? HALF - i : 0;
      a[i + 1] = -i / 8;
    }

  test (N, N, res, a, 2);
  if (res[0] != HALF || res[1] != 0)
    __builtin_abort ();

  test (N, N, res, a, 6);
  if (res[0] != HALF - 4 || res[1] != 0)
    __builtin_abort ();

  test (N, N, res, a, 8);
  if (res[0] != HALF - 6 || res[1] != 0)
    __builtin_abort ();

  test (N, N, res, a, 10);
  if (res[0] != HALF - 8 || res[1] != -1)
    __builtin_abort ();

  test (N, N, res, a, HALF - 2);
  if (res[0] != 4 || res[1] != -HALF / 8 + 1)
    __builtin_abort ();

  test (N, N, res, a, HALF);
  if (res[0] != 2 || res[1] != -HALF / 8 + 1)
    __builtin_abort ();

  test (N, N, res, a, HALF + 2);
  if (res[0] != 0 || res[1] != -HALF / 8)
    __builtin_abort ();

  test (N, N, res, a, HALF + 8);
  if (res[0] != 0 || res[1] != -HALF / 8)
    __builtin_abort ();

  test (N, N, res, a, HALF + 10);
  if (res[0] != 0 || res[1] != -HALF / 8 - 1)
    __builtin_abort ();

  test (N, N, res, a, N);
  if (res[0] != 0 || res[1] != -N / 8 + 1)
    __builtin_abort ();

  test (-1, N, res, a, N);
  if (res[0] != -1 || res[1] != -N / 8 + 1)
    __builtin_abort ();

  test (-1, -N / 8, res, a, N);
  if (res[0] != -1 || res[1] != -N / 8)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "Detected reduction" "vect" } } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target vect_max_reduc } } } */
