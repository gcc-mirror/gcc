/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vint16mf2_t
test_riscv_vfncvt_x_f_w_i16mf2_rm (vfloat32m1_t op1, size_t vl) {
  return __riscv_vfncvt_x_f_w_i16mf2_rm (op1, 0, vl);
}

vint16mf2_t
test_vfncvt_x_f_w_i16mf2_rm_m (vbool32_t mask, vfloat32m1_t op1, size_t vl) {
  return __riscv_vfncvt_x_f_w_i16mf2_rm_m (mask, op1, 1, vl);
}

vint16mf2_t
test_riscv_vfncvt_x_f_w_i16mf2 (vfloat32m1_t op1, size_t vl) {
  return __riscv_vfncvt_x_f_w_i16mf2 (op1, vl);
}

vint16mf2_t
test_vfncvt_x_f_w_i16mf2_m (vbool32_t mask, vfloat32m1_t op1, size_t vl) {
  return __riscv_vfncvt_x_f_w_i16mf2_m (mask, op1, vl);
}

/* { dg-final { scan-assembler-times {vfncvt\.x\.f\.w\s+v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 2 } } */
