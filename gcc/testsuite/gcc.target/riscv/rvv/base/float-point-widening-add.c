/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

typedef float float32_t;

vfloat64m2_t
test_vfwadd_vv_f32m1_rm (vfloat32m1_t op1, vfloat32m1_t op2, size_t vl) {
  return __riscv_vfwadd_vv_f64m2_rm (op1, op2, 0, vl);
}

vfloat64m2_t
test_vfwadd_vv_f32m1_rm_m (vbool32_t mask, vfloat32m1_t op1, vfloat32m1_t op2,
			   size_t vl) {
  return __riscv_vfwadd_vv_f64m2_rm_m (mask, op1, op2, 1, vl);
}

vfloat64m2_t
test_vfwadd_vf_f32m1_rm (vfloat32m1_t op1, float32_t op2, size_t vl) {
  return __riscv_vfwadd_vf_f64m2_rm (op1, op2, 2, vl);
}

vfloat64m2_t
test_vfwadd_vf_f32m1_rm_m (vbool32_t mask, vfloat32m1_t op1, float32_t op2,
			   size_t vl) {
  return __riscv_vfwadd_vf_f64m2_rm_m (mask, op1, op2, 3, vl);
}

vfloat64m2_t
test_vfwadd_wv_f32m1_rm (vfloat64m2_t op1, vfloat32m1_t op2, size_t vl) {
  return __riscv_vfwadd_wv_f64m2_rm (op1, op2, 0, vl);
}

vfloat64m2_t
test_vfwadd_wv_f32m1_rm_m (vbool32_t mask, vfloat64m2_t op1, vfloat32m1_t op2,
			   size_t vl) {
  return __riscv_vfwadd_wv_f64m2_rm_m (mask, op1, op2, 1, vl);
}

vfloat64m2_t
test_vfwadd_wf_f32m1_rm (vfloat64m2_t op1, float32_t op2, size_t vl) {
  return __riscv_vfwadd_wf_f64m2_rm (op1, op2, 2, vl);
}

vfloat64m2_t
test_vfwadd_wf_f32m1_rm_m (vbool32_t mask, vfloat64m2_t op1, float32_t op2,
			   size_t vl) {
  return __riscv_vfwadd_wf_f64m2_rm_m (mask, op1, op2, 3, vl);
}

vfloat64m2_t
test_vfwadd_vv_f32m1 (vfloat32m1_t op1, vfloat32m1_t op2, size_t vl) {
  return __riscv_vfwadd_vv_f64m2 (op1, op2, vl);
}

vfloat64m2_t
test_vfwadd_vv_f32m1_m (vbool32_t mask, vfloat32m1_t op1, vfloat32m1_t op2,
			   size_t vl) {
  return __riscv_vfwadd_vv_f64m2_m (mask, op1, op2, vl);
}

/* { dg-final { scan-assembler-times {vfwadd\.[vw][vf]\s+v[0-9]+,\s*v[0-9]+,\s*[fav]+[0-9]+} 10 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 8 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 8 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 8 } } */
