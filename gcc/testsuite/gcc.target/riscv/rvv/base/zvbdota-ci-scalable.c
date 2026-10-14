/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvqwbdota8i_zvqwbdota16i -mabi=lp64d -mrvv-vector-bits=scalable -O2" } */

#include "riscv_vector.h"

/* The minimum VLEN is not the runtime VLEN.  A caller can guard CI=56.  */
vuint32m8_t
test_u32 (vuint32m8_t acc, vuint8m8_t a, vuint8m1_t b, size_t vl)
{
  if (__riscv_vlenb () < 256)
    return acc;
  return __riscv_vqwbdota_vv_u8m8_u8m1_u32m8 (acc, a, b, 56, vl);
}

vuint64m8_t
test_u64 (vuint64m8_t acc, vuint16m8_t a, vuint16m1_t b, size_t vl)
{
  if (__riscv_vlenb () < 512)
    return acc;
  return __riscv_vqwbdota_vv_u16m8_u16m1_u64m8 (acc, a, b, 56, vl);
}

/* { dg-final { scan-assembler-times {vqwbdotau\.vv} 2 } } */
