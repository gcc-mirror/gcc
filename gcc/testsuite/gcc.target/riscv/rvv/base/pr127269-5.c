/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vuint32m2_t
test (vuint16m2_t a, size_t vl)
{
  vuint16m1_t b = __riscv_vget_v_u16m2_u16m1 (a, 1);
  return __riscv_vwmulu_vv_u32m2 (b, b, vl);
}

/* { dg-final { scan-assembler-times {vwmulu\.vv} 1 } } */
