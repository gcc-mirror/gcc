/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vfloat32m1_t
test_riscv_vfredosum_vs_f32m1_f32m1_rm (vfloat32m1_t op1, vfloat32m1_t op2,
					size_t vl) {
  return __riscv_vfredosum_vs_f32m1_f32m1_rm (op1, op2, 0, vl);
}

vfloat32m1_t
test_vfredosum_vs_f32m1_f32m1_rm_m (vbool32_t mask, vfloat32m1_t op1,
				    vfloat32m1_t op2, size_t vl) {
  return __riscv_vfredosum_vs_f32m1_f32m1_rm_m (mask, op1, op2, 1, vl);
}

vfloat32m1_t
test_riscv_vfredosum_vs_f32m1_f32m1 (vfloat32m1_t op1, vfloat32m1_t op2,
				     size_t vl) {
  return __riscv_vfredosum_vs_f32m1_f32m1 (op1, op2, vl);
}

vfloat32m1_t
test_vfredosum_vs_f32m1_f32m1_m (vbool32_t mask, vfloat32m1_t op1,
				 vfloat32m1_t op2, size_t vl) {
  return __riscv_vfredosum_vs_f32m1_f32m1_m (mask, op1, op2, vl);
}

/* { dg-final { scan-assembler-times {vfredosum\.vs\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 2 } } */
