/* { dg-additional-options "-ffast-math -fdump-tree-optimized" } */

#include "tree-vect.h"

#define N 4

float a[N];

float __attribute__ ((noipa))
f1 (void)
{
  float b[N] = { 0, 1, 1, 1 }, res = 0;
  for (int i = 0; i < N; ++i)
    res += a[i] * b[i];
  return res;
}

float __attribute__ ((noipa))
f2 (void)
{
  float b[N] = { 0, 1, 0, 1 }, res = 0;
  for (int i = 0; i < N; ++i)
    res += a[i] * b[i];
  return res;
}

float __attribute__ ((noipa))
f3 (void)
{
  float b[N] = { 1, 1, 0, 0 }, res = 0;
  for (int i = 0; i < N; ++i)
    res += a[i] * b[i];
  return res;
}

int
main ()
{
  check_vect ();

  for (int i = 0; i < N; ++i)
    a[i] = 0xe0 + i;

  if (f1 () != a[1] + a[2] + a[3]
      || f2 () != a[1] + a[3]
      || f3 () != a[0] + a[1])
    __builtin_abort ();

  return 0;
}
