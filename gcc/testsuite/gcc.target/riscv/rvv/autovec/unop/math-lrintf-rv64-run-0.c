/* { dg-do run { target { riscv_v && rv64 } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include "test-math.h"

#define ARRAY_SIZE 128

float in[ARRAY_SIZE];
long out[ARRAY_SIZE];
long ref[ARRAY_SIZE];

TEST_UNARY_CALL_CVT (float, long, __builtin_lrintf)
TEST_ASSERT (long)

TEST_INIT_CVT (float, 1.2, long, __builtin_lrintf (1.2), 1)
TEST_INIT_CVT (float, -1.2, long, __builtin_lrintf (-1.2), 2)
TEST_INIT_CVT (float, 0.5, long, __builtin_lrintf (0.5), 3)
TEST_INIT_CVT (float, -0.5, long, __builtin_lrintf (-0.5), 4)
TEST_INIT_CVT (float, 0.1, long, __builtin_lrintf (0.1), 5)
TEST_INIT_CVT (float, -0.1, long, __builtin_lrintf (-0.1), 6)
TEST_INIT_CVT (float, 3.0, long, __builtin_lrintf (3.0), 7)
TEST_INIT_CVT (float, -3.0, long, __builtin_lrintf (-3.0), 8)
TEST_INIT_CVT (float, 4503599627370495.5, long, __builtin_lrintf (4503599627370495.5), 9)
TEST_INIT_CVT (float, 4503599627370497.0, long, __builtin_lrintf (4503599627370497.0), 10)
TEST_INIT_CVT (float, -4503599627370495.5, long, __builtin_lrintf (-4503599627370495.5), 11)
TEST_INIT_CVT (float, -4503599627370496.0, long, __builtin_lrintf (-4503599627370496.0), 12)
TEST_INIT_CVT (float, 0.0, long, __builtin_lrintf (-0.0), 13)
TEST_INIT_CVT (float, -0.0, long, __builtin_lrintf (-0.0), 14)
TEST_INIT_CVT (float, 9223372036854774784.0, long, __builtin_lrintf (9223372036854774784.0), 15)
TEST_INIT_CVT (float, 9223372036854775808.0, long, __builtin_lrintf (9223372036854775808.0), 16)
TEST_INIT_CVT (float, -9223372036854775808.0, long, __builtin_lrintf (-9223372036854775808.0), 17)
TEST_INIT_CVT (float, -9223372036854777856.0, long, __builtin_lrintf (-9223372036854777856.0), 18)
TEST_INIT_CVT (float, __builtin_inf (), long, __builtin_lrintf (__builtin_inf ()), 19)
TEST_INIT_CVT (float, -__builtin_inf (), long, __builtin_lrintf (-__builtin_inf ()), 20)
TEST_INIT_CVT (float, __builtin_nan (""), long, 0x7fffffffffffffff, 21)

int
main ()
{
  RUN_TEST_CVT (float, long, 1, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 2, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 3, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 4, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 5, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 6, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 7, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 8, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 9, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 10, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 11, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 12, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 13, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 14, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 15, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 16, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 17, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 18, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 19, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 20, __builtin_lrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, long, 21, __builtin_lrintf, in, out, ref, ARRAY_SIZE);

  return 0;
}
