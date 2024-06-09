/* Test that we do not have error when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vint32m1_t
__attribute__((target("arch=+v")))
test_1 (vint32m1_t a, vint32m1_t b, size_t vl)
{
  return __riscv_vadd_vv_i32m1 (a, b, vl);
}

void
test_2 ()
{
  vint32m1_t a;
}
