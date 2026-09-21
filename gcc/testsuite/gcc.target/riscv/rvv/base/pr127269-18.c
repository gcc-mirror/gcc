/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vint32m2_t
test (vint16m4_t a, vint32m2_t b, size_t vl)
{
  vint16m1_t c = __riscv_vget_v_i16m4_i16m1 (a, 0);
  return __riscv_vwmacc_vx_i32m2 (b, 1, c, vl);
}
