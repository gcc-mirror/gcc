/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3 -mrvv-vector-bits=zvl" } */

#include <assert.h>
#include "compress-1.c"

int
main (void)
{
  vnx4i test_1_x = {0, 1, 2, 4};
  vnx4i test_1_y = {4, 5, 7, 8};
  vnx4i test_1_except = {0, 2, 7, 8};
  vnx4i test_1_real;
  test_1_real = test_1 (test_1_x, test_1_y);
  for (int i = 0; i < 4; i++)
    assert (test_1_real[i] == test_1_except[i]);

  vnx4ui test_2_x = {0, 1, 2, 4};
  vnx4ui test_2_y = {4, 5, 6, 8};
  vnx4ui test_2_except = {0, 2, 6, 8};
  vnx4ui test_2_real;
  test_2_real = test_2 (test_2_x, test_2_y);
  for (int i = 0; i < 4; i++)
    assert (test_2_real[i] == test_2_except[i]);

  return 0;
}
