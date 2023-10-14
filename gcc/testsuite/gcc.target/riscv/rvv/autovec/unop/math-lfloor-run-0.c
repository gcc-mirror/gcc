/* { dg-do run { target { riscv_v && rv64 } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include "test-math.h"

#define ARRAY_SIZE 128

double in[ARRAY_SIZE];
long out[ARRAY_SIZE];
long ref[ARRAY_SIZE];

TEST_UNARY_CALL_CVT (double, long, __builtin_lfloor)
TEST_ASSERT (long)

TEST_INIT_CVT (double, 1.2, long, __builtin_lfloor (1.2), 1)
TEST_INIT_CVT (double, -1.2, long, __builtin_lfloor (-1.2), 2)
TEST_INIT_CVT (double, 0.5, long, __builtin_lfloor (0.5), 3)
TEST_INIT_CVT (double, -0.5, long, __builtin_lfloor (-0.5), 4)
TEST_INIT_CVT (double, 0.1, long, __builtin_lfloor (0.1), 5)
TEST_INIT_CVT (double, -0.1, long, __builtin_lfloor (-0.1), 6)
TEST_INIT_CVT (double, 3.0, long, __builtin_lfloor (3.0), 7)
TEST_INIT_CVT (double, -3.0, long, __builtin_lfloor (-3.0), 8)
TEST_INIT_CVT (double, 4503599627370495.5, long, __builtin_lfloor (4503599627370495.5), 9)
TEST_INIT_CVT (double, 4503599627370497.0, long, __builtin_lfloor (4503599627370497.0), 10)
TEST_INIT_CVT (double, -4503599627370495.5, long, __builtin_lfloor (-4503599627370495.5), 11)
TEST_INIT_CVT (double, -4503599627370496.0, long, __builtin_lfloor (-4503599627370496.0), 12)
TEST_INIT_CVT (double, 0.0, long, __builtin_lfloor (-0.0), 13)
TEST_INIT_CVT (double, -0.0, long, __builtin_lfloor (-0.0), 14)
TEST_INIT_CVT (double, 9223372036854774784.0, long, __builtin_lfloor (9223372036854774784.0), 15)
TEST_INIT_CVT (double, 9223372036854775808.0, long, 0x7fffffffffffffff, 16)
TEST_INIT_CVT (double, -9223372036854775808.0, long, __builtin_lfloor (-9223372036854775808.0), 17)
TEST_INIT_CVT (double, -9223372036854777856.0, long, 0x8000000000000000, 18)
TEST_INIT_CVT (double, __builtin_inf (), long, __builtin_lfloor (__builtin_inf ()), 19)
TEST_INIT_CVT (double, -__builtin_inf (), long, __builtin_lfloor (-__builtin_inf ()), 20)
TEST_INIT_CVT (double, __builtin_nan (""), long, 0x7fffffffffffffff, 21)

/*
   Similar to lround, some reference are hard-code instead of leveraging
   scalar __builtin_lfloor because the return value of a NaN or an infinity,
   or the rounded value is too large to be stored in a long is UNSPECIFIED.
*/

int
main ()
{
  RUN_TEST_CVT (double, long, 1, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 2, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 3, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 4, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 5, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 6, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 7, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 8, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 9, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 10, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 11, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 12, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 13, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 14, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 15, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 16, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 17, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 18, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 19, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 20, __builtin_lfloor, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, long, 21, __builtin_lfloor, in, out, ref, ARRAY_SIZE);

  return 0;
}
