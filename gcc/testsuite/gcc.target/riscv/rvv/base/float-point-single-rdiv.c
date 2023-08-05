/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3 -Wno-psabi" } */

#include "riscv_vector.h"

typedef float float32_t;

vfloat32m1_t
test_vfrdiv_vf_f32m1_rm (vfloat32m1_t op1, float32_t op2, size_t vl) {
  return __riscv_vfrdiv_vf_f32m1_rm (op1, op2, 2, vl);
}

vfloat32m1_t
test_vfrdiv_vf_f32m1_rm_m (vbool32_t mask, vfloat32m1_t op1, float32_t op2,
			   size_t vl) {
  return __riscv_vfrdiv_vf_f32m1_rm_m (mask, op1, op2, 3, vl);
}

vfloat32m1_t
test_riscv_vfrdiv_vf_f32m1 (vfloat32m1_t op1, float32_t op2, size_t vl) {
  return __riscv_vfrdiv_vf_f32m1 (op1, op2, vl);
}

vfloat32m1_t
test_vfrdiv_vf_f32m1_m (vbool32_t mask, vfloat32m1_t op1, float32_t op2,
			size_t vl) {
  return __riscv_vfrdiv_vf_f32m1_m (mask, op1, op2, vl);
}

/* { dg-final { scan-assembler-times {vfrdiv\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[fav]+[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 2 } } */
