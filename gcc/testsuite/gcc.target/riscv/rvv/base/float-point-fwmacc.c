/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

typedef float float32_t;

vfloat64m2_t
test_vfwmacc_vv_f32m1_rm (vfloat64m2_t vd, vfloat32m1_t op1, vfloat32m1_t op2,
			  size_t vl) {
  return __riscv_vfwmacc_vv_f64m2_rm (vd, op1, op2, 0, vl);
}

vfloat64m2_t
test_vfwmacc_vv_f32m1_rm_m (vbool32_t mask, vfloat64m2_t vd, vfloat32m1_t op1,
			    vfloat32m1_t op2, size_t vl) {
  return __riscv_vfwmacc_vv_f64m2_rm_m (mask, vd, op1, op2, 1, vl);
}

vfloat64m2_t
test_vfwmacc_vf_f32m1_rm (vfloat64m2_t vd, float32_t op1, vfloat32m1_t op2,
			  size_t vl) {
  return __riscv_vfwmacc_vf_f64m2_rm (vd, op1, op2, 2, vl);
}

vfloat64m2_t
test_vfwmacc_vf_f32m1_rm_m (vbool32_t mask, vfloat64m2_t vd, float32_t op1,
			    vfloat32m1_t op2, size_t vl) {
  return __riscv_vfwmacc_vf_f64m2_rm_m (mask, vd, op1, op2, 3, vl);
}

vfloat64m2_t
test_vfwmacc_vv_f32m1 (vfloat64m2_t vd, vfloat32m1_t op1, vfloat32m1_t op2,
		       size_t vl) {
  return __riscv_vfwmacc_vv_f64m2 (vd, op1, op2, vl);
}

vfloat64m2_t
test_vfwmacc_vv_f32m1_m (vbool32_t mask, vfloat64m2_t vd, vfloat32m1_t op1,
			 vfloat32m1_t op2, size_t vl) {
  return __riscv_vfwmacc_vv_f64m2_m (mask, vd, op1, op2, vl);
}

/* { dg-final { scan-assembler-times {vfwmacc\.[vw][vf]\s+v[0-9]+,\s*[fav]+[0-9]+,\s*[fav]+[0-9]+} 6 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 4 } } */
