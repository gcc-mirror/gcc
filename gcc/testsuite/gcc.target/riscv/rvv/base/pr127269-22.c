/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */
/* PR target/127269 */

#include "riscv_vector.h"

/* This test is to ensure there is no ICE during compilation.  */

vuint64m4_t
test (vuint32m8_t a, vuint64m4_t b, size_t vl)
{
  vuint32m2_t c = __riscv_vget_v_u32m8_u32m2 (a, 1);
  return __riscv_vwaddu_wv_u64m4 (b, c, vl);
}
