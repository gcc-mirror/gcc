/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vint64m4_t
test (vint64m4_t a, vint32m4_t b, size_t vl)
{
  vint32m2_t c = __riscv_vget_v_i32m4_i32m2 (b, 1);
  return __riscv_vwadd_wv_i64m4 (a, c, vl);
}

/* { dg-final { scan-assembler-times {vwadd\.wv} 1 } } */
