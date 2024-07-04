/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

typedef float float32_t;

vfloat32m1_t
test_riscv_vfnmacc_vv_f32m1_rm (vfloat32m1_t vd, vfloat32m1_t op1,
			        vfloat32m1_t op2, size_t vl) {
  return __riscv_vfnmacc_vv_f32m1_rm (vd, op1, op2, 0, vl);
}

vfloat32m1_t
test_vfnmacc_vv_f32m1_rm_m (vbool32_t mask, vfloat32m1_t vd, vfloat32m1_t op1,
			    vfloat32m1_t op2, size_t vl) {
  return __riscv_vfnmacc_vv_f32m1_rm_m (mask, vd, op1, op2, 1, vl);
}

vfloat32m1_t
test_vfnmacc_vf_f32m1_rm (vfloat32m1_t vd, float32_t op1, vfloat32m1_t op2,
			  size_t vl) {
  return __riscv_vfnmacc_vf_f32m1_rm (vd, op1, op2, 2, vl);
}

vfloat32m1_t
test_vfnmacc_vf_f32m1_rm_m (vfloat32m1_t vd, vbool32_t mask, float32_t op1,
			    vfloat32m1_t op2, size_t vl) {
  return __riscv_vfnmacc_vf_f32m1_rm_m (mask, vd, op1, op2, 3, vl);
}

vfloat32m1_t
test_riscv_vfnmacc_vv_f32m1 (vfloat32m1_t vd, vfloat32m1_t op1,
			     vfloat32m1_t op2, size_t vl) {
  return __riscv_vfnmacc_vv_f32m1 (vd, op1, op2, vl);
}

vfloat32m1_t
test_vfnmacc_vv_f32m1_m (vbool32_t mask, vfloat32m1_t vd, vfloat32m1_t op1,
			 vfloat32m1_t op2, size_t vl) {
  return __riscv_vfnmacc_vv_f32m1_m (mask, vd, op1, op2, vl);
}

/* { dg-final { scan-assembler-times {vfnmacc\.v[vf]\s+v[0-9]+,\s*[fav]+[0-9]+,\s*v[0-9]+} 6 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 4 } } */
