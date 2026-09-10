/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vint32m4_t
test (vint16m4_t a, size_t vl)
{
  vint16m2_t b = __riscv_vget_v_i16m4_i16m2 (a, 0);
  return __riscv_vwadd_vv_i32m4 (b, b, vl);
}

/* { dg-final { scan-assembler-times {vwadd\.vv} 1 } } */
