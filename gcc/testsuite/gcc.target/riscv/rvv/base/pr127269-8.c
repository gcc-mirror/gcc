/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vint64m8_t
test (vint8m8_t a, size_t vl)
{
  vint8m1_t b = __riscv_vget_v_i8m8_i8m1 (a, 3);
  return __riscv_vsext_vf8_i64m8 (b, vl);
}

/* { dg-final { scan-assembler-times {vsext\.vf8} 1 } } */
