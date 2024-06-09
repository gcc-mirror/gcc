/* Test that we do not have error when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vfloat16m1_t
__attribute__((target("arch=+zve32x")))
test_1 (vfloat16m1_t a, vfloat16m1_t b, size_t vl)
{
  return __riscv_vfadd_vv_f16m1 (a, b, vl);
}
