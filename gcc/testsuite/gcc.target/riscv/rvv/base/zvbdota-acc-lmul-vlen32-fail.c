/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvqwbdota8i_zvfwbdota16bf_zvfbfmin_zvfqwbdota8f_zvfbdota32f -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vuint32m1_t
test_vqwbdota_u32m1 (vuint32m1_t vd, vuint8m8_t vs2, vuint8m1_t vs1,
		     size_t vl)
{
  return __riscv_vqwbdota_vv_u8m8_u8m1_u32m1 (vd, vs2, vs1, 0, vl);
  /* { dg-error "requires the zvl256b ISA extension" "" { target *-*-* } .-1 } */
}

vuint32m2_t
test_vqwbdota_u32m2 (vuint32m2_t vd, vuint8m8_t vs2, vuint8m1_t vs1,
		     size_t vl)
{
  return __riscv_vqwbdota_vv_u8m8_u8m1_u32m2 (vd, vs2, vs1, 0, vl);
  /* { dg-error "requires the zvl128b ISA extension" "" { target *-*-* } .-1 } */
}

vuint32m4_t
test_vqwbdota_u32m4 (vuint32m4_t vd, vuint8m8_t vs2, vuint8m1_t vs1,
		     size_t vl)
{
  return __riscv_vqwbdota_vv_u8m8_u8m1_u32m4 (vd, vs2, vs1, 0, vl);
  /* { dg-error "requires the zvl64b ISA extension" "" { target *-*-* } .-1 } */
}

vuint32m8_t
test_vqwbdota_u32m8 (vuint32m8_t vd, vuint8m8_t vs2, vuint8m1_t vs1,
		     size_t vl)
{
  return __riscv_vqwbdota_vv_u8m8_u8m1_u32m8 (vd, vs2, vs1, 0, vl);
}

vfloat32m4_t
test_vfwbdota_f32m4 (vfloat32m4_t vd, vbfloat16m8_t vs2,
		     vbfloat16m1_t vs1, size_t vl)
{
  return __riscv_vfwbdota_vv_bf16m8_bf16m1_f32m4 (vd, vs2, vs1, 0, vl);
  /* { dg-error "requires the zvl64b ISA extension" "" { target *-*-* } .-1 } */
}

vfloat32m4_t
test_vfqwbdota_f32m4 (vfloat32m4_t vd, vfloat8e4m3m8_t vs2,
		      vfloat8e4m3m1_t vs1, size_t vl)
{
  return __riscv_vfqwbdota_vv_f8e4m3m8_f8e4m3m1_f32m4 (vd, vs2, vs1, 0, vl);
  /* { dg-error "requires the zvl64b ISA extension" "" { target *-*-* } .-1 } */
}

vfloat32m4_t
test_vfbdota_f32m4 (vfloat32m4_t vd, vfloat32m8_t vs2,
		    vfloat32m1_t vs1, size_t vl)
{
  return __riscv_vfbdota_vv_f32m4 (vd, vs2, vs1, 0, vl);
  /* { dg-error "requires the zvl64b ISA extension" "" { target *-*-* } .-1 } */
}
