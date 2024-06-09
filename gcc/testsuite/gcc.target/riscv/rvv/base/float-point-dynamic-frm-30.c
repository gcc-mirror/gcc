/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */
// PR113249
/* { dg-additional-options "-fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void
test_float_point_dynamic_frm (float *in, float *out, int n, int cond, size_t vl)
{
  for (int i = 0; i < n; i++)
    {
      if (cond)
        {
          vfloat32m1_t v = __riscv_vle32_v_f32m1 (in + i + 100, vl);
          vfloat32m1_t result = __riscv_vfadd_vv_f32m1_rm (v, v, 3, vl);
          __riscv_vse32_v_f32m1 (out + i + 100, result, vl);
        }
      else
        {
          vfloat32m1_t v = __riscv_vle32_v_f32m1 (in + i + 200, vl);
          vfloat32m1_t result = __riscv_vfadd_vv_f32m1_rm (v, v, 3, vl);
          __riscv_vse32_v_f32m1 (out + i + 200, result, vl);
        }
    }
}

/* { dg-final { scan-assembler-times {vfadd\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[fav]+[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {frrm\s+[axs][0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[axs][0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[01234]} 2 } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[axs][0-9]+,\s*[01234]} } } */
