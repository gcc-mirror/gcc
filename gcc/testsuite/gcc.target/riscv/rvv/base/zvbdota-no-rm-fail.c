/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfwbdota16bf_zvfqwbdota8f_zvfbfmin_zvl512b -mabi=lp64d -O2 -Werror=implicit-function-declaration" } */

#include "riscv_vector.h"

void
test_vfwbdota_rm (vfloat32m8_t vd, vbfloat16m8_t vs2,
		  vbfloat16m1_t vs1, size_t vl)
{
  (void) __riscv_vfwbdota_vv_bf16m8_bf16m1_f32m8_rm (vd, vs2, vs1, 0, __RISCV_FRM_RNE, vl); /* { dg-error "implicit declaration of function" } */
}

void
test_vfqwbdota_rm (vfloat32m8_t vd, vfloat8e4m3m8_t vs2,
		   vfloat8e4m3m1_t vs1, size_t vl)
{
  (void) __riscv_vfqwbdota_vv_f8e4m3m8_f8e4m3m1_f32m8_rm (vd, vs2, vs1, 8, __RISCV_FRM_RNE, vl); /* { dg-error "implicit declaration of function" } */
}

void
test_vfqwbdota_e5m2_rm (vfloat32m8_t vd, vfloat8e5m2m8_t vs2,
			vfloat8e4m3m1_t vs1, size_t vl)
{
  (void) __riscv_vfqwbdota_vv_f8e5m2m8_f8e4m3m1_f32m8_rm (vd, vs2, vs1, 16, __RISCV_FRM_RNE, vl); /* { dg-error "implicit declaration of function" } */
}
