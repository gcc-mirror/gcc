/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vint16m2_t
test (vint8m2_t a, size_t vl)
{
  vint8m1_t b = __riscv_vget_v_i8m2_i8m1 (a, 0);
  return __riscv_vsext_vf2_i16m2 (b, vl);
}

/* { dg-final { scan-assembler-times {vsext\.vf2} 1 } } */
