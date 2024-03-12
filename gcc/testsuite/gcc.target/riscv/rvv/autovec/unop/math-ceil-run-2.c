/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include "test-math.h"

#define ARRAY_SIZE 128

double in[ARRAY_SIZE];
double out[ARRAY_SIZE];
double ref[ARRAY_SIZE];

TEST_UNARY_CALL (double, __builtin_ceil)
TEST_ASSERT (double)

TEST_INIT (double, 1.2, __builtin_ceil (1.2), 1)
TEST_INIT (double, -1.2, __builtin_ceil (-1.2), 2)
TEST_INIT (double, 3.0, __builtin_ceil (3.0), 3)
TEST_INIT (double, 4503599627370495.5, __builtin_ceil (4503599627370495.5), 4)
TEST_INIT (double, 4503599627370497.0, __builtin_ceil (4503599627370497.0), 5)
TEST_INIT (double, 0.0, __builtin_ceil (0.0), 6)
TEST_INIT (double, -0.0, __builtin_ceil (-0.0), 7)
TEST_INIT (double, -4503599627370495.5, __builtin_ceil (-4503599627370495.5), 8)
TEST_INIT (double, -4503599627370496.0, __builtin_ceil (-4503599627370496.0), 9)

int
main ()
{
  RUN_TEST (double, 1, __builtin_ceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 2, __builtin_ceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 3, __builtin_ceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 4, __builtin_ceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 5, __builtin_ceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 6, __builtin_ceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 7, __builtin_ceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 8, __builtin_ceil, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 9, __builtin_ceil, in, out, ref, ARRAY_SIZE);

  return 0;
}
