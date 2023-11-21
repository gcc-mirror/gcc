/* { dg-do run { target { riscv_v } } } */

#define N 0x1100

#include "reduc-9.c"

int
main (void)
{
  float x[N];
  for (int i = 0; i < N; ++i)
    x[i] = ((i + 1) * (i + 2)) & 0xfffff;

  if (add_loop (x, 0, 33) != 33
      || add_loop (x, 11, 30) != 4078
      || add_loop (x, 0x100, 45) != 45001776
      || add_loop (x, 0x11f, 300) != 63369904)
    __builtin_abort ();

  return 0;
}
