/* { dg-require-effective-target vect_float } */

#include "tree-vect.h"

#ifndef TYPE
#define TYPE float
#define FN __builtin_fmaxf
#endif

TYPE __attribute__((noipa))
test (TYPE x, TYPE *ptr, int n)
{
  for (int i = 0; i < n; ++i)
    x = FN (x, ptr[i]);
  return x;
}

#define N 128
#define HALF (N / 2)

int
main (void)
{
  check_vect ();

  TYPE a[N];

  for (int i = 0; i < N; ++i)
    a[i] = i;

  if (test (-1, a, 1) != 0)
    __builtin_abort ();
  if (test (-1, a, 64) != 63)
    __builtin_abort ();
  if (test (-1, a, 65) != 64)
    __builtin_abort ();
  if (test (-1, a, 66) != 65)
    __builtin_abort ();
  if (test (-1, a, 67) != 66)
    __builtin_abort ();
  if (test (-1, a, 128) != 127)
    __builtin_abort ();
  if (test (127, a, 128) != 127)
    __builtin_abort ();
  if (test (128, a, 128) != 128)
    __builtin_abort ();

  for (int i = 0; i < N; ++i)
    a[i] = -i;

  if (test (-60, a, 4) != 0)
    __builtin_abort ();
  if (test (0, a, 4) != 0)
    __builtin_abort ();
  if (test (1, a, 4) != 1)
    __builtin_abort ();

  for (int i = 0; i < HALF; ++i)
    {
      a[i] = i;
      a[HALF + i] = HALF - i;
    }

  if (test (0, a, HALF - 16) != HALF - 17)
    __builtin_abort ();
  if (test (0, a, HALF - 2) != HALF - 3)
    __builtin_abort ();
  if (test (0, a, HALF - 1) != HALF - 2)
    __builtin_abort ();
  if (test (0, a, HALF) != HALF - 1)
    __builtin_abort ();
  if (test (0, a, HALF + 1) != HALF)
    __builtin_abort ();
  if (test (0, a, HALF + 2) != HALF)
    __builtin_abort ();
  if (test (0, a, HALF + 3) != HALF)
    __builtin_abort ();
  if (test (0, a, HALF + 16) != HALF)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "Detected reduction" "vect" } } */
/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" { target vect_max_reduc } } } */
