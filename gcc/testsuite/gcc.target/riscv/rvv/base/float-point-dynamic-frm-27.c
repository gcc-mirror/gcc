/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

void
test_float_point_dynamic_frm (vfloat32m1_t op1, vfloat32m1_t op2, size_t vl,
                              float *out)
{
  vfloat32m1_t result = op1;

  result = __riscv_vfadd_vv_f32m1 (op1, result, vl);

  if (vl % 4 == 0)
    {
      result = __riscv_vfadd_vv_f32m1 (op1, result, vl);
      *(vfloat32m1_t *) out = result;
    }
  else
    {
      result = __riscv_vfadd_vv_f32m1 (op2, result, vl);
      *(vfloat32m1_t *) (out + 16) = result;
    }
}

/* { dg-final { scan-assembler-times {vfadd\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[fav]+[0-9]+} 3 } } */
/* { dg-final { scan-assembler-not {frrm\s+[axs][0-9]+} } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[axs][0-9]+,\s*[01234]} } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[01234]} } } */
/* { dg-final { scan-assembler-not {fsrm\s+[axs][0-9]+} } } */
