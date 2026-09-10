/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vuint32m2_t
test (vfloat16m2_t a, size_t vl)
{
  vfloat16m1_t b = __riscv_vget_v_f16m2_f16m1 (a, 1);
  return __riscv_vfwcvt_rtz_xu_f_v_u32m2 (b, vl);
}

/* { dg-final { scan-assembler-times {vfwcvt\.rtz\.xu\.f\.v} 1 } } */
