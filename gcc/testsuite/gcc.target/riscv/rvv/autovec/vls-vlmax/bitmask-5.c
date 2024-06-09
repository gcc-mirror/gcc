/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -mrvv-max-lmul=m2 -O3" } */

#include <stdint-gcc.h>
#include <assert.h>

#define N 32

int
main ()
{
  int8_t mask[N] = {0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1,
		    0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1};
  int8_t out[N] = {0};
  for (int8_t i = 0; i < N; ++i)
    if (mask[i])
      out[i] = i;
  for (int8_t i = 0; i < N; ++i)
    {
      if (mask[i])
	assert (out[i] == i);
      else
	assert (out[i] == 0);
    }
}
