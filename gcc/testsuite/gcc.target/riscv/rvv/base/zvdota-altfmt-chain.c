/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvqwdota8i -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vint32m1_t
test_altfmt_chain (vbool8_t mask, vint32m1_t acc, vint8m1_t a,
		   vint8m1_t b, vuint8m1_t c, size_t vl)
{
  acc = __riscv_vqwdota_vv_i8m1_i8m1_i32m1_m (mask, acc, a, b, vl);
  acc = __riscv_vqwdota_vv_i8m1_u8m1_i32m1_m (mask, acc, a, c, vl);
  return __riscv_vqwdota_vv_i8m1_i8m1_i32m1_m (mask, acc, a, b, vl);
}

/* { dg-final { scan-assembler-times {vqwdotas\.vv} 3 } } */
/* { dg-final { scan-assembler-times {e8alt,\s*m1,} 2 } } */
/* { dg-final { scan-assembler-times {e8,\s*m1,} 1 } } */
