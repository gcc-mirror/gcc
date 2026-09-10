/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vfloat64m2_t
test (vfloat32m2_t a, size_t vl)
{
  vfloat32m1_t b = __riscv_vget_v_f32m2_f32m1 (a, 1);
  return __riscv_vfwcvt_f_f_v_f64m2 (b, vl);
}

/* { dg-final { scan-assembler-times {vfwcvt\.f\.f\.v} 1 } } */
