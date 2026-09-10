/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vfloat32m4_t
test (vint16m4_t a, size_t vl)
{
  vint16m2_t b = __riscv_vget_v_i16m4_i16m2 (a, 1);
  return __riscv_vfwcvt_f_x_v_f32m4 (b, vl);
}

/* { dg-final { scan-assembler-times {vfwcvt\.f\.x\.v} 1 } } */
