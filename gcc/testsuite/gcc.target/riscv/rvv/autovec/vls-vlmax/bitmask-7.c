/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -mrvv-max-lmul=m8 -O3" } */

#include <stdint-gcc.h>
#include <assert.h>

#define N 128

int
main ()
{
  uint8_t mask[N]
    = {0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
       0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
       0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
       0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
       0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
       0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1};
  uint8_t out[N] = {0};
  for (uint8_t i = 0; i < N; ++i)
    if (mask[i])
      out[i] = i;
  for (uint8_t i = 0; i < N; ++i)
    {
      if (mask[i])
	assert (out[i] == i);
      else
	assert (out[i] == 0);
    }
}
