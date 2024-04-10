/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vuint64m2_t
test_riscv_vfwcvt_xu_f_v_u64m2_rm (vfloat32m1_t op1, size_t vl) {
  return __riscv_vfwcvt_xu_f_v_u64m2_rm (op1, 0, vl);
}

vuint64m2_t
test_vfwcvt_xu_f_v_u64m2_rm_m (vbool32_t mask, vfloat32m1_t op1, size_t vl) {
  return __riscv_vfwcvt_xu_f_v_u64m2_rm_m (mask, op1, 1, vl);
}

vuint64m2_t
test_riscv_vfwcvt_xu_f_v_u64m2 (vfloat32m1_t op1, size_t vl) {
  return __riscv_vfwcvt_xu_f_v_u64m2 (op1, vl);
}

vuint64m2_t
test_vfwcvt_xu_f_v_u64m2_m (vbool32_t mask, vfloat32m1_t op1, size_t vl) {
  return __riscv_vfwcvt_xu_f_v_u64m2_m (mask, op1, vl);
}

/* { dg-final { scan-assembler-times {vfwcvt\.xu\.f\.v\s+v[0-9]+,\s*v[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 2 } } */
