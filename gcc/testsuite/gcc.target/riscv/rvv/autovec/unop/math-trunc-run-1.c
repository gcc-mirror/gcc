/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include "test-math.h"

#define ARRAY_SIZE 128

float in[ARRAY_SIZE];
float out[ARRAY_SIZE];
float ref[ARRAY_SIZE];

TEST_UNARY_CALL (float, __builtin_truncf)
TEST_ASSERT (float)

TEST_INIT (float, 1.2, __builtin_truncf (1.2), 1)
TEST_INIT (float, -1.2,__builtin_truncf (-1.2), 2)
TEST_INIT (float, 3.0, __builtin_truncf (3.0), 3)
TEST_INIT (float, 8388607.5, __builtin_truncf (8388607.5), 4)
TEST_INIT (float, 8388609.0, __builtin_truncf (8388609.0), 5)
TEST_INIT (float, 0.0, __builtin_truncf (0.0), 6)
TEST_INIT (float, -0.0, __builtin_truncf (-0.0), 7)
TEST_INIT (float, -8388607.5, __builtin_truncf (-8388607.5), 8)
TEST_INIT (float, -8388608.0, __builtin_truncf (-8388608.0), 9)

int
main ()
{
  RUN_TEST (float, 1, __builtin_truncf, in, out, ref, ARRAY_SIZE);
  RUN_TEST (float, 2, __builtin_truncf, in, out, ref, ARRAY_SIZE);
  RUN_TEST (float, 3, __builtin_truncf, in, out, ref, ARRAY_SIZE);
  RUN_TEST (float, 4, __builtin_truncf, in, out, ref, ARRAY_SIZE);
  RUN_TEST (float, 5, __builtin_truncf, in, out, ref, ARRAY_SIZE);
  RUN_TEST (float, 6, __builtin_truncf, in, out, ref, ARRAY_SIZE);
  RUN_TEST (float, 7, __builtin_truncf, in, out, ref, ARRAY_SIZE);
  RUN_TEST (float, 8, __builtin_truncf, in, out, ref, ARRAY_SIZE);
  RUN_TEST (float, 9, __builtin_truncf, in, out, ref, ARRAY_SIZE);

  return 0;
}
