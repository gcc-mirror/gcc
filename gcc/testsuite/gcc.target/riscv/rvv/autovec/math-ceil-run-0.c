/* { dg-do run { target { riscv_vector } } } */
/* { dg-additional-options "-march=rv64gcv_zvfh -std=c2x -mabi=lp64d -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#include "test-math.h"

#define ARRAY_SIZE 128

_Float16 in[ARRAY_SIZE];
_Float16 out[ARRAY_SIZE];
_Float16 ref[ARRAY_SIZE];

TEST_CEIL (_Float16, __builtin_ceilf16)
TEST_ASSERT (_Float16)

TEST_INIT (_Float16, 1.2, 2.0, 1)
TEST_INIT (_Float16, -1.2, -1.0, 2)
TEST_INIT (_Float16, 3.0, 3.0, 3)
TEST_INIT (_Float16, 1023.5, 1024.0, 4)
TEST_INIT (_Float16, 1025.0, 1025.0, 5)
TEST_INIT (_Float16, 0.0, 0.0, 6)
TEST_INIT (_Float16, -0.0, -0.0, 7)
TEST_INIT (_Float16, -1023.5, -1023.0, 8)
TEST_INIT (_Float16, -1024.0, -1024.0, 9)

int
main ()
{
  RUN_TEST (_Float16, 1, __builtin_ceilf16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (_Float16, 2, __builtin_ceilf16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (_Float16, 3, __builtin_ceilf16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (_Float16, 4, __builtin_ceilf16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (_Float16, 5, __builtin_ceilf16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (_Float16, 6, __builtin_ceilf16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (_Float16, 7, __builtin_ceilf16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (_Float16, 8, __builtin_ceilf16, in, out, ref, ARRAY_SIZE);
  RUN_TEST (_Float16, 9, __builtin_ceilf16, in, out, ref, ARRAY_SIZE);

  return 0;
}
