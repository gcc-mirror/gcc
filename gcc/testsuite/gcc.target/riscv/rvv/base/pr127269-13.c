/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vfloat32m4_t
test (vfloat16m4_t a, size_t vl)
{
  vfloat16m2_t b = __riscv_vget_v_f16m4_f16m2 (a, 1);
  return __riscv_vfwcvt_f_f_v_f32m4 (b, vl);
}

/* { dg-final { scan-assembler-times {vfwcvt\.f\.f\.v} 1 } } */
