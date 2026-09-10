/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vint32m2_t
test (vint16m2_t a, vint32m2_t c, size_t vl)
{
  vint16m1_t b = __riscv_vget_v_i16m2_i16m1 (a, 1);
  return __riscv_vwmacc_vv_i32m2 (c, b, b, vl);
}

/* { dg-final { scan-assembler-times {vwmacc\.vv} 1 } } */
