/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include <stdint-gcc.h>
#include "test-math.h"

#define ARRAY_SIZE 128

float in[ARRAY_SIZE];
int64_t out[ARRAY_SIZE];
int64_t ref[ARRAY_SIZE];

TEST_UNARY_CALL_CVT (float, int64_t, __builtin_llrintf)
TEST_ASSERT (int64_t)


TEST_INIT_CVT (float, 1.2, int64_t, __builtin_llrintf (1.2), 1)
TEST_INIT_CVT (float, -1.2, int64_t, __builtin_llrintf (-1.2), 2)
TEST_INIT_CVT (float, 0.5, int64_t, __builtin_llrintf (0.5), 3)
TEST_INIT_CVT (float, -0.5, int64_t, __builtin_llrintf (-0.5), 4)
TEST_INIT_CVT (float, 0.1, int64_t, __builtin_llrintf (0.1), 5)
TEST_INIT_CVT (float, -0.1, int64_t, __builtin_llrintf (-0.1), 6)
TEST_INIT_CVT (float, 3.0, int64_t, __builtin_llrintf (3.0), 7)
TEST_INIT_CVT (float, -3.0, int64_t, __builtin_llrintf (-3.0), 8)
TEST_INIT_CVT (float, 2147483520.0, int64_t, __builtin_llrintf (2147483520.0), 9)
TEST_INIT_CVT (float, 2147483648.0, int64_t, __builtin_llrintf (2147483648.0), 10)
TEST_INIT_CVT (float, -2147483648.0, int64_t, __builtin_llrintf (-2147483648.0), 11)
TEST_INIT_CVT (float, -2147483904.0, int64_t, __builtin_llrintf (-2147483904.0), 12)
TEST_INIT_CVT (float, 0.0, int64_t, __builtin_llrintf (-0.0), 13)
TEST_INIT_CVT (float, -0.0, int64_t, __builtin_llrintf (-0.0), 14)
TEST_INIT_CVT (float, 9223372036854775807.0, int64_t, __builtin_llrintf (9223372036854775807.0), 15)
TEST_INIT_CVT (float, 9223373136366403584.0, int64_t, __builtin_llrintf (9223373136366403584.0), 16)
TEST_INIT_CVT (float, -9223372036854775807.0, int64_t, __builtin_llrintf (-9223372036854775807.0), 17)
TEST_INIT_CVT (float, -9223373136366403584.0, int64_t, __builtin_llrintf (-9223373136366403584.0), 18)
TEST_INIT_CVT (float, __builtin_inf (), int64_t, __builtin_llrintf (__builtin_inff ()), 19)
TEST_INIT_CVT (float, -__builtin_inf (), int64_t, __builtin_llrintf (-__builtin_inff ()), 20)
TEST_INIT_CVT (float, __builtin_nanf (""), int64_t, 0x7fffffffffffffff, 21)

int
main ()
{
  RUN_TEST_CVT (float, int64_t, 1, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 2, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 3, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 4, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 5, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 6, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 7, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 8, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 9, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 10, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 11, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 12, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 13, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 14, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 15, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 16, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 17, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 18, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 19, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 20, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 21, __builtin_llrintf, in, out, ref, ARRAY_SIZE);

  return 0;
}
