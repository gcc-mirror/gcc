/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include "test-math.h"

#define ARRAY_SIZE 128

double in[ARRAY_SIZE];
int out[ARRAY_SIZE];
int ref[ARRAY_SIZE];

TEST_UNARY_CALL_CVT (double, int, __builtin_irint)
TEST_ASSERT (int)

TEST_INIT_CVT (double, 1.2, int, __builtin_irint (1.2), 1)
TEST_INIT_CVT (double, -1.2, int, __builtin_irint (-1.2), 2)
TEST_INIT_CVT (double, 0.5, int, __builtin_irint (0.5), 3)
TEST_INIT_CVT (double, -0.5, int, __builtin_irint (-0.5), 4)
TEST_INIT_CVT (double, 0.1, int, __builtin_irint (0.1), 5)
TEST_INIT_CVT (double, -0.1, int, __builtin_irint (-0.1), 6)
TEST_INIT_CVT (double, 3.0, int, __builtin_irint (3.0), 7)
TEST_INIT_CVT (double, -3.0, int, __builtin_irint (-3.0), 8)
TEST_INIT_CVT (double, 4503599627370495.5, int, __builtin_irint (4503599627370495.5), 9)
TEST_INIT_CVT (double, 4503599627370497.0, int, __builtin_irint (4503599627370497.0), 10)
TEST_INIT_CVT (double, -4503599627370495.5, int, __builtin_irint (-4503599627370495.5), 11)
TEST_INIT_CVT (double, -4503599627370496.0, int, __builtin_irint (-4503599627370496.0), 12)
TEST_INIT_CVT (double, 0.0, int, __builtin_irint (-0.0), 13)
TEST_INIT_CVT (double, -0.0, int, __builtin_irint (-0.0), 14)
TEST_INIT_CVT (double, 9223372036854774784.0, int, __builtin_irint (9223372036854774784.0), 15)
TEST_INIT_CVT (double, 9223372036854775808.0, int, __builtin_irint (9223372036854775808.0), 16)
TEST_INIT_CVT (double, -9223372036854775808.0, int, __builtin_irint (-9223372036854775808.0), 17)
TEST_INIT_CVT (double, -9223372036854777856.0, int, __builtin_irint (-9223372036854777856.0), 18)
TEST_INIT_CVT (double, __builtin_inf (), int, __builtin_irint (__builtin_inf ()), 19)
TEST_INIT_CVT (double, -__builtin_inf (), int, __builtin_irint (-__builtin_inf ()), 20)
TEST_INIT_CVT (double, __builtin_nan (""), int, 0x7fffffff, 21)

int
main ()
{
  RUN_TEST_CVT (double, int, 1, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 2, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 3, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 4, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 5, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 6, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 7, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 8, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 9, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 10, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 11, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 12, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 13, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 14, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 15, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 16, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 17, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 18, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 19, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 20, __builtin_irint, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (double, int, 21, __builtin_irint, in, out, ref, ARRAY_SIZE);

  return 0;
}
