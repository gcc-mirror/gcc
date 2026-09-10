/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vint32m2_t
test (vint16m2_t a, size_t vl)
{
  vint16m1_t b = __riscv_vget_v_i16m2_i16m1 (a, 0);
  return __riscv_vwcvt_x_x_v_i32m2 (b, vl);
}

/* { dg-final { scan-assembler-times {vwcvt\.x\.x\.v} 1 } } */
