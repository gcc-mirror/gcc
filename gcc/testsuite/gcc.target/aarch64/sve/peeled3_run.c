/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O3 -mautovec-preference=sve-only -msve-vector-bits=scalable" } */

#include "peeled3.c"

#define N 128

int a[N] __attribute__ ((aligned (64)));
int b[N] __attribute__ ((aligned (64)));

static void
clear_arrays (void)
{
  for (int i = 0; i < N; ++i)
    {
      a[i] = 0;
      b[i] = 0;
    }
}

int
main (void)
{
  clear_arrays ();
  b[16] = 1;
  if (c (a, b, 16) != 1)
    __builtin_abort ();

  clear_arrays ();
  b[15] = 1;
  if (c (a, b, 16) != 0)
    __builtin_abort ();

  return 0;
}
