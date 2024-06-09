/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include <stdint-gcc.h>
#include <assert.h>

typedef int8_t v16qi __attribute__ ((vector_size (16)));

void __attribute__ ((noinline, noclone)) 
foo (int8_t *out, int8_t x, int8_t y)
{
  v16qi v = {y, y, y, y, y, y, y, y, x, x, x, x, x, x, x, x};
  *(v16qi *) out = v;
}

int
main ()
{
  int8_t x = -107;
  int8_t y = 93;
  int8_t out[16] = {0};
  foo (out, x, y);
  for (int i = 0; i < 16; ++i)
    {
      if (i < 8)
	assert (out[i] == 93);
      else
	assert (out[i] == -107);
    }
}
