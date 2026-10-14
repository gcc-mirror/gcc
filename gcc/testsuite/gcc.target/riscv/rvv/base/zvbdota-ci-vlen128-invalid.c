/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvqwbdota8i_zvqwbdota16i_zvfbdota32f -mabi=lp64d -mrvv-vector-bits=zvl -O2" } */

#include "riscv_vector.h"

/* Even an m8 accumulator cannot enlarge the hardware destination group.  */
vuint32m8_t
test_u32 (vuint32m8_t acc, vuint8m8_t a, vuint8m1_t b, size_t vl)
{
  return __riscv_vqwbdota_vv_u8m8_u8m1_u32m8 (acc, a, b, 8, vl); /* { dg-error {range 0 to 0 for a 128-bit VLEN} } */
}

vuint64m8_t
test_u64 (vuint64m8_t acc, vuint16m8_t a, vuint16m1_t b, size_t vl)
{
  return __riscv_vqwbdota_vv_u16m8_u16m1_u64m8 (acc, a, b, 8, vl); /* { dg-error {range 0 to 0 for a 128-bit VLEN} } */
}

vfloat32m8_t
test_masked (vbool4_t mask, vfloat32m8_t acc, vfloat32m8_t a,
	     vfloat32m1_t b, size_t vl)
{
  return __riscv_vfbdota (mask, acc, a, b, 8, vl); /* { dg-error {range 0 to 0 for a 128-bit VLEN} } */
}

vfloat32m8_t
test_masked_rm (vbool4_t mask, vfloat32m8_t acc, vfloat32m8_t a,
		vfloat32m1_t b, size_t vl)
{
  return __riscv_vfbdota (mask, acc, a, b, 8, __RISCV_FRM_RNE, vl); /* { dg-error {range 0 to 0 for a 128-bit VLEN} } */
}
