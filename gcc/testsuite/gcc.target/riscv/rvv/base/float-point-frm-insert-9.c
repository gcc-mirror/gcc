/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

void
test_float_point_frm_static (float *out, vfloat32m1_t op1, vfloat32m1_t op2,
			     size_t vl)
{
  vfloat32m1_t result = __riscv_vfadd_vv_f32m1_rm (op1, op2, 2, vl);

  asm volatile (
    "fsrmi 4"
    :
    :
    :"frm"
  );

  result = __riscv_vfadd_vv_f32m1_rm (op1, result, 3, vl);
  *(vfloat32m1_t *)out = result;
}

/* { dg-final { scan-assembler-times {vfadd\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[fav]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 3 } } */
