/* { dg-do run { target { riscv_v && rv32 } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include "test-math.h"

#define ARRAY_SIZE 128

double in[ARRAY_SIZE];
long out[ARRAY_SIZE];
long ref[ARRAY_SIZE];

TEST_UNARY_CALL_CVT (double, long, __builtin_lceil)
TEST_ASSERT (long)

TEST_INIT_CVT (double, 0.0, long, __builtin_lceil (-0.0), 1)
TEST_INIT_CVT (double, -0.0, long, __builtin_lceil (-0.0), 2)
TEST_INIT_CVT (double, 1.2, long, __builtin_lceil (1.2), 3)
TEST_INIT_CVT (double, -1.2, long, __builtin_lceil (-1.2), 4)
TEST_INIT_CVT (double, 1.5, long, __builtin_lceil (1.5), 5)
TEST_INIT_CVT (double, -1.5, long, __builtin_lceil (-1.5), 6)
TEST_INIT_CVT (double, 1.8, long, __builtin_lceil (1.8), 7)
TEST_INIT_CVT (double, -1.8, long, __builtin_lceil (-1.8), 8)
TEST_INIT_CVT (double, 0.2, long, __builtin_lceil (0.2), 9)
TEST_INIT_CVT (double, -0.2, long, __builtin_lceil (-0.2), 10)
TEST_INIT_CVT (double, 0.5, long, __builtin_lceil (0.5), 11)
TEST_INIT_CVT (double, -0.5, long, __builtin_lceil (-0.5), 12)
TEST_INIT_CVT (double, 0.8, long, __builtin_lceil (0.8), 13)
TEST_INIT_CVT (double, -0.8, long, __builtin_lceil (-0.8), 14)
TEST_INIT_CVT (double, 4.0, long, __builtin_lceil (4.0), 15)
TEST_INIT_CVT (double, -4.0, long, __builtin_lceil (-4.0), 16)
TEST_INIT_CVT (double, 4503599627370495.5, long, __builtin_lceil (4503599627370495.5), 17)
TEST_INIT_CVT (double, 4503599627370497.0, long, __builtin_lceil (4503599627370497.0), 18)
TEST_INIT_CVT (double, -4503599627370495.5, long, __builtin_lceil (-4503599627370495.5), 19)
TEST_INIT_CVT (double, -4503599627370496.0, long, __builtin_lceil (-4503599627370496.0), 20)
TEST_INIT_CVT (double, 2147483647.0, long, __builtin_lceil (2147483647.0), 21)
TEST_INIT_CVT (double, -2147483648.0, long, __builtin_lceil (-2147483648.0), 22)
TEST_INIT_CVT (double, 2147483647.0000002384185791, long, 0x7fffffff, 23)
TEST_INIT_CVT (double, -2147483648.0000004768371582, long, __builtin_lceil (-2147483648.0000004768371582), 24)
TEST_INIT_CVT (double, 9223372036854774784.0, long, __builtin_lceil (9223372036854774784.0), 25)
TEST_INIT_CVT (double, 9223372036854775808.0, long, 0x7fffffff, 26)
TEST_INIT_CVT (double, -9223372036854775808.0, long, __builtin_lceil (-9223372036854775808.0), 27)
TEST_INIT_CVT (double, -9223372036854777856.0, long, 0x80000000, 28)
TEST_INIT_CVT (double, __builtin_inf (), long, __builtin_lceil (__builtin_inf ()), 29)
TEST_INIT_CVT (double, -__builtin_inf (), long, __builtin_lceil (-__builtin_inf ()), 30)
TEST_INIT_CVT (double, __builtin_nan (""), long, 0x7fffffff, 31)

long
main ()
{
  RUN_TEST_CVT (double, long, 1, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 2, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 3, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 4, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 5, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 6, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 7, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 8, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 9, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 10, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 11, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 12, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 13, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 14, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 15, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 16, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 17, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 18, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 19, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 20, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 21, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 22, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 23, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 24, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 25, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 26, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 27, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 28, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 29, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 30, __builtin_lceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 31, __builtin_lceil, in, out, ref, ARRAY_SIZE);

  return 0;
}
