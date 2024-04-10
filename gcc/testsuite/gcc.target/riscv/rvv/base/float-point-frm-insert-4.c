/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vfloat32m1_t
test_float_point_frm_insert (vfloat32m1_t op1, vfloat32m1_t op2, size_t vl,
			     size_t count) {
  vfloat32m1_t tmp = op1, result;

  result = __riscv_vfadd_vv_f32m1_rm (result, op1, 1, vl);
  result = __riscv_vfadd_vv_f32m1_rm (result, op2, 1, vl);

  for (int i = 0; i < count; i++) {
    tmp = __riscv_vfadd_vv_f32m1_rm (op1, tmp, 1, vl + i);
    result = __riscv_vfrsqrt7_v_f32m1 (tmp, vl + i);
  }

  return __riscv_vfadd_vv_f32m1_rm (result, op2, 1, vl);
}

/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v+[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 1 } } */
