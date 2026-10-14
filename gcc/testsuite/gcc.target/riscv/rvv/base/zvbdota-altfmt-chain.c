/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvqwbdota8i_zvl512b -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vint32m1_t
test_altfmt_chain (vint32m1_t acc, vint8m8_t a, vint8m1_t b,
		   vuint8m1_t c, size_t vl)
{
  acc = __riscv_vqwbdota_vv_i8m8_i8m1_i32m1 (acc, a, b, 0, vl);
  acc = __riscv_vqwbdota_vv_i8m8_u8m1_i32m1 (acc, a, c, 0, vl);
  return __riscv_vqwbdota_vv_i8m8_i8m1_i32m1 (acc, a, b, 0, vl);
}

/* { dg-final { scan-assembler-times {vqwbdotas\.vv} 3 } } */
/* { dg-final { scan-assembler-times {e8alt,\s*m1,} 2 } } */
/* { dg-final { scan-assembler-times {e8,\s*m1,} 1 } } */
