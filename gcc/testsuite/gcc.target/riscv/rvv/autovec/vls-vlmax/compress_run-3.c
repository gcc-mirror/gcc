/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3 -mrvv-vector-bits=zvl" } */

#include <assert.h>
#include "compress-3.c"

int
main (void)
{
  vnx16i test_1_x = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  vnx16i test_1_y
    = {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  vnx16i test_1_except
    = {1, 3, 4, 6, 7, 8, 14, 15, 20, 21, 22, 23, 24, 25, 26, 27};
  vnx16i test_1_real;
  test_1_real = test_1 (test_1_x, test_1_y);
  for (int i = 0; i < 16; i++)
    assert (test_1_real[i] == test_1_except[i]);

  vnx8i test_2_x = {0, 1, 2, 3, 4, 5, 6, 7};
  vnx8i test_2_y = {8, 9, 10, 11, 12, 13, 14, 15};
  vnx8i test_2_except = {0, 1, 2, 5, 10, 11, 12, 13};
  vnx8i test_2_real;
  test_2_real = test_2 (test_2_x, test_2_y);
  for (int i = 0; i < 8; i++)
    assert (test_2_real[i] == test_2_except[i]);

  vnx4i test_3_x = {0, 1, 2, 3};
  vnx4i test_3_y = {4, 5, 6, 7};
  vnx4i test_3_except = {0, 2, 5, 6};
  vnx4i test_3_real;
  test_3_real = test_3 (test_3_x, test_3_y);
  for (int i = 0; i < 4; i++)
    assert (test_3_real[i] == test_3_except[i]);

  vnx16ui test_4_x = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  vnx16ui test_4_y
    = {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31};
  vnx16ui test_4_except
    = {1, 3, 4, 6, 7, 8, 14, 15, 20, 21, 22, 23, 24, 25, 26, 27};
  vnx16ui test_4_real;
  test_4_real = test_4 (test_4_x, test_4_y);
  for (int i = 0; i < 16; i++)
    assert (test_4_real[i] == test_4_except[i]);

  vnx8ui test_5_x = {0, 1, 2, 3, 4, 5, 6, 7};
  vnx8ui test_5_y = {8, 9, 10, 11, 12, 13, 14, 15};
  vnx8ui test_5_except = {0, 1, 2, 5, 10, 11, 12, 13};
  vnx8ui test_5_real;
  test_5_real = test_5 (test_5_x, test_5_y);
  for (int i = 0; i < 8; i++)
    assert (test_5_real[i] == test_5_except[i]);

  vnx4ui test_6_x = {0, 1, 2, 3};
  vnx4ui test_6_y = {4, 5, 6, 7};
  vnx4ui test_6_except = {0, 2, 5, 6};
  vnx4ui test_6_real;
  test_6_real = test_6 (test_6_x, test_6_y);
  for (int i = 0; i < 4; i++)
    assert (test_6_real[i] == test_6_except[i]);

  vnx8f test_7_x = {0, 1, 2, 3, 4, 5, 6, 7};
  vnx8f test_7_y = {8, 9, 10, 11, 12, 13, 14, 15};
  vnx8f test_7_except = {0, 1, 2, 5, 10, 11, 12, 13};
  vnx8f test_7_real;
  test_7_real = test_7 (test_7_x, test_7_y);
  for (int i = 0; i < 8; i++)
    assert (test_7_real[i] == test_7_except[i]);

  vnx4f test_8_x = {0, 1, 2, 3};
  vnx4f test_8_y = {4, 5, 6, 7};
  vnx4f test_8_except = {0, 2, 5, 6};
  vnx4f test_8_real;
  test_8_real = test_8 (test_8_x, test_8_y);
  for (int i = 0; i < 4; i++)
    assert (test_8_real[i] == test_8_except[i]);

  return 0;
}
