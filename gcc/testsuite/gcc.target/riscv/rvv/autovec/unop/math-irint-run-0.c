/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include "test-math.h"

#define ARRAY_SIZE 128

float in[ARRAY_SIZE];
int out[ARRAY_SIZE];
int ref[ARRAY_SIZE];

TEST_UNARY_CALL_CVT (float, int, __builtin_irintf)
TEST_ASSERT (int)

TEST_INIT_CVT (float, 1.2, int, __builtin_irintf (1.2), 1)
TEST_INIT_CVT (float, -1.2, int, __builtin_irintf (-1.2), 2)
TEST_INIT_CVT (float, 0.5, int, __builtin_irintf (0.5), 3)
TEST_INIT_CVT (float, -0.5, int, __builtin_irintf (-0.5), 4)
TEST_INIT_CVT (float, 0.1, int, __builtin_irintf (0.1), 5)
TEST_INIT_CVT (float, -0.1, int, __builtin_irintf (-0.1), 6)
TEST_INIT_CVT (float, 3.0, int, __builtin_irintf (3.0), 7)
TEST_INIT_CVT (float, -3.0, int, __builtin_irintf (-3.0), 8)
TEST_INIT_CVT (float, 8388607.5, int, __builtin_irintf (8388607.5), 9)
TEST_INIT_CVT (float, 8388609.0, int, __builtin_irintf (8388609.0), 10)
TEST_INIT_CVT (float, -8388607.5, int, __builtin_irintf (-8388607.5), 11)
TEST_INIT_CVT (float, -8388609.0, int, __builtin_irintf (-8388609.0), 12)
TEST_INIT_CVT (float, 0.0, int, __builtin_irintf (-0.0), 13)
TEST_INIT_CVT (float, -0.0, int, __builtin_irintf (-0.0), 14)
TEST_INIT_CVT (float, 2147483520.0, int, __builtin_irintf (2147483520.0), 15)
TEST_INIT_CVT (float, 2147483648.0, int, __builtin_irintf (2147483648.0), 16)
TEST_INIT_CVT (float, -2147483648.0, int, __builtin_irintf (-2147483648.0), 17)
TEST_INIT_CVT (float, -2147483904.0, int, __builtin_irintf (-2147483904.0), 18)
TEST_INIT_CVT (float, __builtin_inf (), int, __builtin_irintf (__builtin_inff ()), 19)
TEST_INIT_CVT (float, -__builtin_inf (), int, __builtin_irintf (-__builtin_inff ()), 20)
TEST_INIT_CVT (float, __builtin_nanf (""), int, 0x7fffffff, 21)

int
main ()
{
  RUN_TEST_CVT (float, int, 1, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 2, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 3, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 4, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 5, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 6, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 7, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 8, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 9, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 10, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 11, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 12, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 13, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 14, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 15, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 16, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 17, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 18, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 19, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 20, __builtin_irintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int, 21, __builtin_irintf, in, out, ref, ARRAY_SIZE);

  return 0;
}
