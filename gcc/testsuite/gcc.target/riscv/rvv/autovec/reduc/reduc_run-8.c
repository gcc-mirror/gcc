/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -ffast-math -fno-vect-cost-model" } */

#define N 0x1100

#include "reduc-8.c"

int
main (void)
{
  int x[N];
  for (int i = 0; i < N; ++i)
    x[i] = ((i + 1) * (i + 2)) & 0xfffff;

  if (add_loop (x, 0, 33) != 33
      || add_loop (x, 11, 30) != 4078
      || add_loop (x, 0x100, 45) != 45001773
      || add_loop (x, 0x11f, 300) != 63369900)
    __builtin_abort ();

  return 0;
}
