/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include "test-math.h"

#define ARRAY_SIZE 128

double in[ARRAY_SIZE];
int out[ARRAY_SIZE];
int ref[ARRAY_SIZE];

TEST_UNARY_CALL_CVT (double, int, __builtin_ifloor)
TEST_ASSERT (int)

TEST_INIT_CVT (double, 0.0, int, __builtin_ifloor (-0.0), 1)
TEST_INIT_CVT (double, -0.0, int, __builtin_ifloor (-0.0), 2)
TEST_INIT_CVT (double, 1.2, int, __builtin_ifloor (1.2), 3)
TEST_INIT_CVT (double, -1.2, int, __builtin_ifloor (-1.2), 4)
TEST_INIT_CVT (double, 1.5, int, __builtin_ifloor (1.5), 5)
TEST_INIT_CVT (double, -1.5, int, __builtin_ifloor (-1.5), 6)
TEST_INIT_CVT (double, 1.8, int, __builtin_ifloor (1.8), 7)
TEST_INIT_CVT (double, -1.8, int, __builtin_ifloor (-1.8), 8)
TEST_INIT_CVT (double, 0.2, int, __builtin_ifloor (0.2), 9)
TEST_INIT_CVT (double, -0.2, int, __builtin_ifloor (-0.2), 10)
TEST_INIT_CVT (double, 0.5, int, __builtin_ifloor (0.5), 11)
TEST_INIT_CVT (double, -0.5, int, __builtin_ifloor (-0.5), 12)
TEST_INIT_CVT (double, 0.8, int, __builtin_ifloor (0.8), 13)
TEST_INIT_CVT (double, -0.8, int, __builtin_ifloor (-0.8), 14)
TEST_INIT_CVT (double, 4.0, int, __builtin_ifloor (4.0), 15)
TEST_INIT_CVT (double, -4.0, int, __builtin_ifloor (-4.0), 16)
TEST_INIT_CVT (double, 4503599627370495.5, int, __builtin_ifloor (4503599627370495.5), 17)
TEST_INIT_CVT (double, 4503599627370497.0, int, __builtin_ifloor (4503599627370497.0), 18)
TEST_INIT_CVT (double, -4503599627370495.5, int, __builtin_ifloor (-4503599627370495.5), 19)
TEST_INIT_CVT (double, -4503599627370496.0, int, __builtin_ifloor (-4503599627370496.0), 20)
TEST_INIT_CVT (double, 2147483647.0, int, __builtin_ifloor (2147483647.0), 21)
TEST_INIT_CVT (double, -2147483648.0, int, __builtin_ifloor (-2147483648.0), 22)
TEST_INIT_CVT (double, 2147483647.0000002384185791, int, __builtin_ifloor (2147483647.0000002384185791), 23)
TEST_INIT_CVT (double, -2147483648.0000004768371582, int, 0x80000000, 24)
TEST_INIT_CVT (double, 9223372036854774784.0, int, __builtin_ifloor (9223372036854774784.0), 25)
TEST_INIT_CVT (double, 9223372036854775808.0, int, 0x7fffffff, 26)
TEST_INIT_CVT (double, -9223372036854775808.0, int, __builtin_ifloor (-9223372036854775808.0), 27)
TEST_INIT_CVT (double, -9223372036854777856.0, int, 0x80000000, 28)
TEST_INIT_CVT (double, __builtin_inf (), int, __builtin_ifloor (__builtin_inf ()), 29)
TEST_INIT_CVT (double, -__builtin_inf (), int, __builtin_ifloor (-__builtin_inf ()), 30)
TEST_INIT_CVT (double, __builtin_nan (""), int, 0x7fffffff, 31)

int
main ()
{
  RUN_TEST_CVT (double, int, 1, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 2, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 3, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 4, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 5, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 6, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 7, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 8, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 9, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 10, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 11, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 12, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 13, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 14, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 15, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 16, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 17, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 18, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 19, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 20, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 21, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 22, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 23, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 24, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 25, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 26, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 27, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 28, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 29, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 30, __builtin_ifloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 31, __builtin_ifloor, in, out, ref, ARRAY_SIZE);

  return 0;
}
