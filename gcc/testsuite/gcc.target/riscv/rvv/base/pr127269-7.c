/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vuint32m4_t
test (vuint8m4_t a, size_t vl)
{
  vuint8m1_t b = __riscv_vget_v_u8m4_u8m1 (a, 2);
  return __riscv_vzext_vf4_u32m4 (b, vl);
}

/* { dg-final { scan-assembler-times {vzext\.vf4} 1 } } */
