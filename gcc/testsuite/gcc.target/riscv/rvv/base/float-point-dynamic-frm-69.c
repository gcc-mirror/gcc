/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

extern size_t normalize_vl_1 (size_t vl);

vfloat32m1_t
test_float_point_dynamic_frm (vfloat32m1_t op1, vfloat32m1_t op2,
			     unsigned count, size_t vl)
{
  vfloat32m1_t result;
  vfloat32m1_t f32_res = op1;
  vint32m1_t i32_res = __riscv_vreinterpret_v_f32m1_i32m1 (op2);

  if (count & vl == 0x1f)
    i32_res = __riscv_vadd_vv_i32m1 (i32_res, i32_res, vl);
  else
    vl = normalize_vl_1 (vl);

  f32_res = __riscv_vreinterpret_v_i32m1_f32m1 (i32_res);
  result = __riscv_vfadd_vv_f32m1_rm (f32_res, op2, 4, vl);

  return result;
}

/* { dg-final { scan-assembler-times {vfadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 2 } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[axs][0-9]+,\s*[01234]} } } */
