/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include <stdint-gcc.h>
#include "test-math.h"

#define ARRAY_SIZE 128

float in[ARRAY_SIZE];
int64_t out[ARRAY_SIZE];
int64_t ref[ARRAY_SIZE];

TEST_UNARY_CALL_CVT (float, int64_t, __builtin_llfloorf)
TEST_ASSERT (int64_t)

TEST_INIT_CVT (float, 0.0, int64_t, __builtin_llfloorf (-0.0), 1)
TEST_INIT_CVT (float, -0.0, int64_t, __builtin_llfloorf (-0.0), 2)
TEST_INIT_CVT (float, 1.2, int64_t, __builtin_llfloorf (1.2), 3)
TEST_INIT_CVT (float, -1.2, int64_t, __builtin_llfloorf (-1.2), 4)
TEST_INIT_CVT (float, 1.5, int64_t, __builtin_llfloorf (1.5), 5)
TEST_INIT_CVT (float, -1.5, int64_t, __builtin_llfloorf (-1.5), 6)
TEST_INIT_CVT (float, 1.8, int64_t, __builtin_llfloorf (1.8), 7)
TEST_INIT_CVT (float, -1.8, int64_t, __builtin_llfloorf (-1.8), 8)
TEST_INIT_CVT (float, 0.2, int64_t, __builtin_llfloorf (0.2), 9)
TEST_INIT_CVT (float, -0.2, int64_t, __builtin_llfloorf (-0.2), 10)
TEST_INIT_CVT (float, 0.5, int64_t, __builtin_llfloorf (0.5), 11)
TEST_INIT_CVT (float, -0.5, int64_t, __builtin_llfloorf (-0.5), 12)
TEST_INIT_CVT (float, 0.8, int64_t, __builtin_llfloorf (0.8), 13)
TEST_INIT_CVT (float, -0.8, int64_t, __builtin_llfloorf (-0.8), 14)
TEST_INIT_CVT (float, 4.0, int64_t, __builtin_llfloorf (4.0), 15)
TEST_INIT_CVT (float, -4.0, int64_t, __builtin_llfloorf (-4.0), 16)
TEST_INIT_CVT (float, 8388607.5, int64_t, __builtin_llfloorf (8388607.5), 17)
TEST_INIT_CVT (float, 8388609.0, int64_t, __builtin_llfloorf (8388609.0), 18)
TEST_INIT_CVT (float, -8388607.5, int64_t, __builtin_llfloorf (-8388607.5), 19)
TEST_INIT_CVT (float, -8388609.0, int64_t, __builtin_llfloorf (-8388609.0), 20)
TEST_INIT_CVT (float, 2147483520.0, int64_t, __builtin_llfloorf (2147483520.0), 21)
TEST_INIT_CVT (float, 2147483648.0, int64_t, __builtin_llfloorf (2147483648.0), 22)
TEST_INIT_CVT (float, -2147483648.0, int64_t, __builtin_llfloorf (-2147483648.0), 23)
TEST_INIT_CVT (float, -2147483904.0, int64_t, __builtin_llfloorf (-2147483904.0), 24)
TEST_INIT_CVT (float, 9223371487098961920.0, int64_t, __builtin_llfloorf (9223371487098961920.0), 25)
TEST_INIT_CVT (float, 9223372036854775808.0, int64_t, 0x7fffffffffffffff, 26)
TEST_INIT_CVT (float, -9223372036854775808.0, int64_t, __builtin_llfloorf (-9223372036854775808.0), 27)
TEST_INIT_CVT (float, -9223373136366403584.0, int64_t, 0x8000000000000000, 28)
TEST_INIT_CVT (float, __builtin_inf (), int64_t, __builtin_llfloorf (__builtin_inf ()), 29)
TEST_INIT_CVT (float, -__builtin_inf (), int64_t, __builtin_llfloorf (-__builtin_inf ()), 30)
TEST_INIT_CVT (float, __builtin_nan (""), int64_t, 0x7fffffffffffffff, 31)

int64_t
main ()
{
  RUN_TEST_CVT (float, int64_t, 1, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 2, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 3, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 4, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 5, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 6, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 7, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 8, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 9, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 10, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 11, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 12, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 13, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 14, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 15, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 16, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 17, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 18, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 19, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 20, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 21, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 22, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 23, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 24, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 25, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 26, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 27, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 28, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 29, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 30, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 31, __builtin_llfloorf, in, out, ref, ARRAY_SIZE);

  return 0;
}
