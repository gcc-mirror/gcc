/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include <stdint-gcc.h>
#include <assert.h>
#define N 16

int
main ()
{
  int16_t mask[N] = {0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1};
  int16_t out[N] = {0};
  for (int16_t i = 0; i < N; ++i)
    if (mask[i])
      out[i] = i;
  for (int16_t i = 0; i < N; ++i)
    {
      if (mask[i])
	assert (out[i] == i);
      else
	assert (out[i] == 0);
    }
}
