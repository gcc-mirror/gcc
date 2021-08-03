/* { dg-do run { target aarch64_sve256_hw } } */
/* { dg-options "-O3 -msve-vector-bits=256 --param vect-partial-vector-usage=1" } */

#define N 0x1100

#include "reduc_9.c"

int
main (void)
{
  unsigned short x[N];
  for (int i = 0; i < N; ++i)
    x[i] = (i + 1) * (i + 2);

  if (add_loop (x) != 20480
      || max_loop (x) != 65504
      || or_loop (x) != 0xfffe
      || eor_loop (x) != 0xa000)
    __builtin_abort ();

  for (int i = 0; i < N; ++i)
    x[i] = ~x[i];

  if (min_loop (x) != 31
      || and_loop (x) != 1)
    __builtin_abort ();

  return 0;
}
