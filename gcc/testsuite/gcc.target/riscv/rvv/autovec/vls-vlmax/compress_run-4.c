/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3 -mrvv-vector-bits=zvl" } */

#include <assert.h>
#include "compress-4.c"

int
main (void)
{
  vnx32i test_1_x
    = {0,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15, 16,
       17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32};
  vnx32i test_1_y
    = {32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
       48, 49, 50, 51, 52, 53, 54, 55, 57, 58, 59, 60, 61, 62, 63, 64};
  vnx32i test_1_except
    = {4,  5,  7,  8,  10, 11, 15, 16, 18, 21, 22, 23, 25, 28, 30, 32,
       34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49};
  vnx32i test_1_real;
  test_1_real = test_1 (test_1_x, test_1_y);
  for (int i = 0; i < 32; i++)
    assert (test_1_real[i] == test_1_except[i]);

  vnx16i test_2_x = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
  vnx16i test_2_y
    = {16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 28, 29, 30, 31, 32};
  vnx16i test_2_except
    = {0, 3, 5, 7, 10, 11, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23};
  vnx16i test_2_real;
  test_2_real = test_2 (test_2_x, test_2_y);
  for (int i = 0; i < 16; i++)
    assert (test_2_real[i] == test_2_except[i]);

  vnx8i test_3_x = {0, 1, 2, 4, 5, 6, 7, 8};
  vnx8i test_3_y = {8, 10, 11, 12, 13, 14, 15, 16};
  vnx8i test_3_except = {1, 2, 6, 8, 12, 13, 14, 15};
  vnx8i test_3_real;
  test_3_real = test_3 (test_3_x, test_3_y);
  for (int i = 0; i < 8; i++)
    assert (test_3_real[i] == test_3_except[i]);

  vnx4i test_4_x = {0, 2, 3, 4};
  vnx4i test_4_y = {4, 5, 7, 8};
  vnx4i test_4_except = {2, 3, 5, 7};
  vnx4i test_4_real;
  test_4_real = test_4 (test_4_x, test_4_y);
  for (int i = 0; i < 4; i++)
    assert (test_4_real[i] == test_4_except[i]);

  vnx32ui test_5_x
    = {0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
       16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32};
  vnx32ui test_5_y
    = {32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
       48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 60, 61, 62, 63, 64};
  vnx32ui test_5_except
    = {3,  4,  6,  7,  9,  10, 14, 15, 17, 20, 21, 22, 24, 27, 29, 32,
       34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49};
  vnx32ui test_5_real;
  test_5_real = test_5 (test_5_x, test_5_y);
  for (int i = 0; i < 32; i++)
    assert (test_5_real[i] == test_5_except[i]);

  vnx16ui test_6_x = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16};
  vnx16ui test_6_y
    = {16, 17, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32};
  vnx16ui test_6_except
    = {0, 3, 5, 7, 10, 11, 15, 16, 16, 17, 19, 20, 21, 22, 23, 24};
  vnx16ui test_6_real;
  test_6_real = test_6 (test_6_x, test_6_y);
  for (int i = 0; i < 16; i++)
    assert (test_6_real[i] == test_6_except[i]);

  vnx8ui test_7_x = {0, 2, 3, 4, 5, 6, 7, 8};
  vnx8ui test_7_y = {8, 9, 10, 12, 13, 14, 15, 16};
  vnx8ui test_7_except = {2, 3, 6, 8, 12, 13, 14, 15};
  vnx8ui test_7_real;
  test_7_real = test_7 (test_7_x, test_7_y);
  for (int i = 0; i < 8; i++)
    assert (test_7_real[i] == test_7_except[i]);

  vnx4ui test_8_x = {0, 2, 3, 4};
  vnx4ui test_8_y = {5, 6, 7, 8};
  vnx4ui test_8_except = {2, 3, 6, 7};
  vnx4ui test_8_real;
  test_8_real = test_8 (test_8_x, test_8_y);
  for (int i = 0; i < 4; i++)
    assert (test_8_real[i] == test_8_except[i]);

  vnx16f test_9_x = {0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16};
  vnx16f test_9_y
    = {16, 17, 18, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32};
  vnx16f test_9_except
    = {0, 3, 6, 8, 11, 12, 15, 16, 16, 17, 18, 20, 21, 22, 23, 24};
  vnx16f test_9_real;
  test_9_real = test_9 (test_9_x, test_9_y);
  for (int i = 0; i < 16; i++)
    assert (test_9_real[i] == test_9_except[i]);

  vnx8f test_10_x = {0, 1, 2, 3, 4, 5, 6, 7};
  vnx8f test_10_y = {8, 9, 10, 12, 13, 14, 15, 16};
  vnx8f test_10_except = {1, 2, 5, 7, 12, 13, 14, 15};
  vnx8f test_10_real;
  test_10_real = test_10 (test_10_x, test_10_y);
  for (int i = 0; i < 8; i++)
    assert (test_10_real[i] == test_10_except[i]);

  vnx4f test_11_x = {0, 2, 3, 4};
  vnx4f test_11_y = {4, 6, 7, 8};
  vnx4f test_11_except = {2, 3, 6, 7};
  vnx4f test_11_real;
  test_11_real = test_11 (test_11_x, test_11_y);
  for (int i = 0; i < 4; i++)
    assert (test_11_real[i] == test_11_except[i]);

  return 0;
}
