/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vfloat32m1_t
test_float_point_frm_insert (vfloat32m1_t op1, vfloat32m1_t op2, size_t vl) {
  vfloat32m1_t v1 = __riscv_vfadd_vv_f32m1_rm (op1, op2, 0, vl);
  vfloat32m1_t v2 = __riscv_vfmin_vv_f32m1 (op1, v1, vl);
  return __riscv_vfadd_vv_f32m1_rm (v1, v2, 1, vl);
}

/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 2 } } */
