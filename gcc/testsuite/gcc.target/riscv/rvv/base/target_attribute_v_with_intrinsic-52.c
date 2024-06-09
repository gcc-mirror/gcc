/* Test that we do not have error when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vuint32m2_t
__attribute__((target("arch=+zvksed")))
test_1 (vuint32m2_t a, vuint32m2_t b, size_t vl)
{
  return __riscv_vsm4r_vv_u32m2 (a, b, vl);
}
