/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O3 -mautovec-preference=sve-only -msve-vector-bits=scalable" } */

#include "peeled1.c"

static void
clear_arrays (void)
{
  for (int i = 0; i < 100; ++i)
    {
      b[i] = 0;
      e[i] = 0;
    }
}

int
main (void)
{
  clear_arrays ();
  if (c (99) != 1)
    __builtin_abort ();

  clear_arrays ();
  b[37] = 1;
  if (c (99) != 0)
    __builtin_abort ();

  clear_arrays ();
  e[45] = 1;
  if (c (99) != 0)
    __builtin_abort ();

  return 0;
}
