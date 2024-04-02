/* Test that we do not have error when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vfloat32m1_t
__attribute__((target("arch=+zve64x")))
test_1 (vfloat32m1_t a, vfloat32m1_t b, size_t vl)
{
  return __riscv_vfadd_vv_f32m1 (a, b, vl);
}
