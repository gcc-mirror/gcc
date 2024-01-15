/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vfloat32m1_t
test_riscv_vfncvt_f_x_w_f32m1_rm (vint64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_x_w_f32m1_rm (op1, 0, vl);
}

vfloat32m1_t
test_vfncvt_f_x_w_f32m1_rm_m (vbool32_t mask, vint64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_x_w_f32m1_rm_m (mask, op1, 1, vl);
}

vfloat32m1_t
test_riscv_vfncvt_f_xu_w_f32m1_rm (vuint64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_xu_w_f32m1_rm (op1, 0, vl);
}

vfloat32m1_t
test_vfncvt_f_xu_w_f32m1_rm_m (vbool32_t mask, vuint64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_xu_w_f32m1_rm_m (mask, op1, 1, vl);
}

vfloat32m1_t
test_riscv_vfncvt_f_f_w_f32m1_rm (vfloat64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_f_w_f32m1_rm (op1, 0, vl);
}

vfloat32m1_t
test_vfncvt_f_f_w_f32m1_rm_m (vbool32_t mask, vfloat64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_f_w_f32m1_rm_m (mask, op1, 1, vl);
}

vfloat32m1_t
test_riscv_vfncvt_f_x_w_f32m1 (vint64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_x_w_f32m1 (op1, vl);
}

vfloat32m1_t
test_vfncvt_f_x_w_f32m1_m (vbool32_t mask, vint64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_x_w_f32m1_m (mask, op1, vl);
}

vfloat32m1_t
test_riscv_vfncvt_f_xu_w_f32m1 (vuint64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_xu_w_f32m1 (op1, vl);
}

vfloat32m1_t
test_vfncvt_f_xu_w_f32m1_m (vbool32_t mask, vuint64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_xu_w_f32m1_m (mask, op1, vl);
}

vfloat32m1_t
test_riscv_vfncvt_f_f_w_f32m1 (vfloat64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_f_w_f32m1 (op1, vl);
}

vfloat32m1_t
test_vfncvt_f_f_w_f32m1_m (vbool32_t mask, vfloat64m2_t op1, size_t vl) {
  return __riscv_vfncvt_f_f_w_f32m1_m (mask, op1, vl);
}

/* { dg-final { scan-assembler-times {vfncvt\.f\.[xuf]+\.w\s+v[0-9]+,\s*v[0-9]+} 12 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 6 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 6 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 6 } } */
