/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include "test-math.h"

#define ARRAY_SIZE 128

double in[ARRAY_SIZE];
double out[ARRAY_SIZE];
double ref[ARRAY_SIZE];

static double
get_ref_nearbyint (double val)
{
  set_rm (FRM_RNE);

  return __builtin_nearbyint (val);
}

TEST_UNARY_CALL (double, __builtin_nearbyint)
TEST_ASSERT (double)

TEST_INIT (double, 1.2, get_ref_nearbyint (1.2), 1)
TEST_INIT (double, -1.8, get_ref_nearbyint (-1.8), 2)
TEST_INIT (double, 3.0, get_ref_nearbyint (3.0), 3)
TEST_INIT (double, 4503599627370495.5, get_ref_nearbyint (4503599627370495.5), 4)
TEST_INIT (double, 4503599627370497.0, get_ref_nearbyint (4503599627370497.0), 5)
TEST_INIT (double, 0.0, get_ref_nearbyint (0.0), 6)
TEST_INIT (double, -0.0, get_ref_nearbyint (-0.0), 7)
TEST_INIT (double, -4503599627370495.5, get_ref_nearbyint (-4503599627370495.5), 8)
TEST_INIT (double, -4503599627370496.0, get_ref_nearbyint (-4503599627370496.0), 9)

int
main ()
{
  set_rm (FRM_RNE);

  RUN_TEST (double, 1, __builtin_nearbyint, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 2, __builtin_nearbyint, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 3, __builtin_nearbyint, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 4, __builtin_nearbyint, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 5, __builtin_nearbyint, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 6, __builtin_nearbyint, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 7, __builtin_nearbyint, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 8, __builtin_nearbyint, in, out, ref, ARRAY_SIZE);
  RUN_TEST (double, 9, __builtin_nearbyint, in, out, ref, ARRAY_SIZE);

  return 0;
}
