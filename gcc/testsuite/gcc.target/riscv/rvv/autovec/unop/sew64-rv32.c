/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include <stdint-gcc.h>
#include "test-math.h"

#define ARRAY_SIZE 128

float in[ARRAY_SIZE];
int64_t out[ARRAY_SIZE];
int64_t ref[ARRAY_SIZE];

TEST_UNARY_CALL_CVT (float, int64_t, __builtin_llrintf)
TEST_ASSERT (int64_t)


TEST_INIT_CVT (float, __builtin_inf (), int64_t, __builtin_llrintf (__builtin_inff ()), 19)
TEST_INIT_CVT (float, -__builtin_inf (), int64_t, __builtin_llrintf (-__builtin_inff ()), 20)
TEST_INIT_CVT (float, __builtin_nanf (""), int64_t, 0x7fffffffffffffff, 21)

int
main ()
{
  RUN_TEST_CVT (float, int64_t, 19, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 20, __builtin_llrintf, in, out, ref, ARRAY_SIZE);
  RUN_TEST_CVT (float, int64_t, 21, __builtin_llrintf, in, out, ref, ARRAY_SIZE);

  return 0;
}
