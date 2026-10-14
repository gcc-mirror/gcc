/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfwdota16bf_zvfqwdota8f_zvfbfmin -mabi=lp64d -O2 -Werror=implicit-function-declaration" } */

#include "riscv_vector.h"

void
test_vfwdota_rm (vfloat32m1_t vd, vbfloat16mf2_t vs2,
		 vbfloat16mf2_t vs1, size_t vl)
{
  (void) __riscv_vfwdota_vv_bf16mf2_bf16mf2_f32m1_rm (vd, vs2, vs1, __RISCV_FRM_RNE, vl); /* { dg-error "implicit declaration of function" } */
}

void
test_vfqwdota_rm (vfloat32m1_t vd, vfloat8e4m3mf4_t vs2,
		  vfloat8e4m3mf4_t vs1, size_t vl)
{
  (void) __riscv_vfqwdota_vv_f8e4m3mf4_f8e4m3mf4_f32m1_rm (vd, vs2, vs1, __RISCV_FRM_RNE, vl); /* { dg-error "implicit declaration of function" } */
}

void
test_vfqwdota_e5m2_rm (vfloat32m1_t vd, vfloat8e5m2mf4_t vs2,
		       vfloat8e4m3mf4_t vs1, size_t vl)
{
  (void) __riscv_vfqwdota_vv_f8e5m2mf4_f8e4m3mf4_f32m1_rm (vd, vs2, vs1, __RISCV_FRM_RNE, vl); /* { dg-error "implicit declaration of function" } */
}
