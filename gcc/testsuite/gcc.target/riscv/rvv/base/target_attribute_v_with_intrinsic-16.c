/* Test that we do not have error when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vfloat32m1_t
__attribute__((target("arch=+v,+zvfhmin")))
test_1 (vfloat16mf2_t a, size_t vl)
{
  return __riscv_vfwcvt_f_f_v_f32m1 (a, vl);
}
