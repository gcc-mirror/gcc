/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvqwbdota8i_zvqwbdota16i_zvl512b -mabi=lp64d -mrvv-vector-bits=zvl -O2" } */

#include "riscv_vector.h"

vuint32m1_t
test_u32 (vuint32m1_t acc, vuint8m8_t a, vuint8m1_t b, size_t vl)
{
  return __riscv_vqwbdota_vv_u8m8_u8m1_u32m1 (acc, a, b, 8, vl);
}

vuint64m1_t
test_u64 (vuint64m1_t acc, vuint16m8_t a, vuint16m1_t b, size_t vl)
{
  return __riscv_vqwbdota_vv_u16m8_u16m1_u64m1 (acc, a, b, 0, vl);
}

/* { dg-final { scan-assembler-times {vqwbdotau\.vv} 2 } } */
