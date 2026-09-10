/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vint32m8_t
test (vint16m8_t a, size_t vl)
{
  vint16m4_t b = __riscv_vget_v_i16m8_i16m4 (a, 0);
  vint16m4_t c = __riscv_vget_v_i16m8_i16m4 (a, 1);
  return __riscv_vwsub_vv_i32m8 (b, c, vl);
}

/* { dg-final { scan-assembler-times {vwsub\.vv} 1 } } */
