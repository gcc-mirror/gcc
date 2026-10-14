/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvqwbdota8i -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vuint32m8_t
test_ci_4 (vuint32m8_t vd, vuint8m8_t vs2, vuint8m1_t vs1, size_t vl)
{
  return __riscv_vqwbdota_vv_u8m8_u8m1_u32m8 (vd, vs2, vs1, 4, vl);
  /* { dg-error "argument 4 of '__riscv_vqwbdota_vv_u8m8_u8m1_u32m8' must be a constant multiple of 8 in the range 0 to 56" "" { target *-*-* } .-1 } */
}

vint32m8_t
test_ci_64 (vint32m8_t vd, vuint8m8_t vs2, vint8m1_t vs1, size_t vl)
{
  return __riscv_vqwbdota_vv_u8m8_i8m1_i32m8 (vd, vs2, vs1, 64, vl);
  /* { dg-error "argument 4 of '__riscv_vqwbdota_vv_u8m8_i8m1_i32m8' must be a constant multiple of 8 in the range 0 to 56" "" { target *-*-* } .-1 } */
}

vint32m8_t
test_ci_minus_8 (vint32m8_t vd, vint8m8_t vs2, vuint8m1_t vs1, size_t vl)
{
  return __riscv_vqwbdota_vv_i8m8_u8m1_i32m8 (vd, vs2, vs1, -8, vl);
  /* { dg-error "argument 4 of '__riscv_vqwbdota_vv_i8m8_u8m1_i32m8' must be a constant multiple of 8 in the range 0 to 56" "" { target *-*-* } .-1 } */
}

vuint32m8_t
test_ci_variable (vuint32m8_t vd, vuint8m8_t vs2, vuint8m1_t vs1,
		  size_t ci, size_t vl)
{
  return __riscv_vqwbdota_vv_u8m8_u8m1_u32m8 (vd, vs2, vs1, ci, vl);
  /* { dg-error "argument 4 of .* must be an integer constant expression" "" { target *-*-* } .-1 } */
}
