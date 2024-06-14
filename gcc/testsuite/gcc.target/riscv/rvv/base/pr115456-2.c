/* Test there is no ICE when compile.  */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mrvv-vector-bits=zvl -mabi=lp64d -O3 -ftree-vectorize" } */

#include <assert.h>
#include <stdint-gcc.h>

typedef _Float16 vnx4f __attribute__ ((vector_size (8)));

vnx4f __attribute__ ((noinline, noclone))
test_5 (vnx4f x, vnx4f y)
{
  return __builtin_shufflevector (x, y, 1, 3, 6, 7);
}

int
main (void)
{
  vnx4f test_5_x = {0, 1, 3, 4};
  vnx4f test_5_y = {4, 5, 6, 7};
  vnx4f test_5_except = {1, 4, 6, 7};
  vnx4f test_5_real;
  test_5_real = test_5 (test_5_x, test_5_y);

  for (int i = 0; i < 4; i++)
    assert (test_5_real[i] == test_5_except[i]);

  return 0;
}

/* { dg-final { scan-assembler-times {call\s+__extendhfsf2} 8 } } */
