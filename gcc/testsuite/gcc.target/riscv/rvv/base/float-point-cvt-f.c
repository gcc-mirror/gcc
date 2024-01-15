/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vfloat32m1_t
test_riscv_vfcvt_f_x_v_f32m1_rm (vint32m1_t op1, size_t vl) {
  return __riscv_vfcvt_f_x_v_f32m1_rm (op1, 0, vl);
}

vfloat32m1_t
test_riscv_vfcvt_f_x_v_f32m1_rm_m (vbool32_t mask, vint32m1_t op1, size_t vl) {
  return __riscv_vfcvt_f_x_v_f32m1_rm_m (mask, op1, 0, vl);
}

vfloat32m1_t
test_riscv_vfcvt_f_xu_v_f32m1_rm (vuint32m1_t op1, size_t vl) {
  return __riscv_vfcvt_f_xu_v_f32m1_rm (op1, 0, vl);
}

vfloat32m1_t
test_riscv_vfcvt_f_xu_v_f32m1_rm_m (vbool32_t mask, vuint32m1_t op1,
				    size_t vl) {
  return __riscv_vfcvt_f_xu_v_f32m1_rm_m (mask, op1, 0, vl);
}

vfloat32m1_t
test_riscv_vfcvt_f_x_v_f32m1 (vint32m1_t op1, size_t vl) {
  return __riscv_vfcvt_f_x_v_f32m1 (op1, vl);
}

vfloat32m1_t
test_vfcvt_f_x_v_f32m1_m (vbool32_t mask, vint32m1_t op1, size_t vl) {
  return __riscv_vfcvt_f_x_v_f32m1_m (mask, op1, vl);
}

vfloat32m1_t
test_riscv_vfcvt_f_xu_v_f32m1 (vuint32m1_t op1, size_t vl) {
  return __riscv_vfcvt_f_xu_v_f32m1 (op1, vl);
}

vfloat32m1_t
test_vfcvt_f_x_vu_f32m1_m (vbool32_t mask, vuint32m1_t op1, size_t vl) {
  return __riscv_vfcvt_f_xu_v_f32m1_m (mask, op1, vl);
}

/* { dg-final { scan-assembler-times {vfcvt\.f\.x[u]?\.v\s+v[0-9]+,\s*v[0-9]+} 8 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 4 } } */
