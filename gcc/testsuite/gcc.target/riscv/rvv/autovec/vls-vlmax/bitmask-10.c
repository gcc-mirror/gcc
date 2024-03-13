/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3 -mrvv-max-lmul=m2" } */
#include <stdint-gcc.h>
#include <assert.h>
#define N 16

int
main ()
{
  int mask[N] = {0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1};
  int8_t out[N] = {0};
  for (int i = 0; i < N; ++i)
    if (mask[i])
      out[i] = i;
  for (int i = 0; i < N; ++i)
    {
      if (mask[i])
	assert (out[i] == i);
      else
	assert (out[i] == 0);
    }
}
