/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void f (void * in, void *out, int32_t x, int n, int m)
{
  vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
  vint32m1_t v2 = __riscv_vle32_v_i32m1_tu (v, in + 100, 4);
  vint32m1_t v3 = __riscv_vaadd_vx_i32m1 (v2, 0, __RISCV_VXRM_RDN, 4);
  v3 = __riscv_vaadd_vx_i32m1 (v3, 3, __RISCV_VXRM_RDN, 4);
  __riscv_vse32_v_i32m1 (out + 100, v3, 4);
}

/* { dg-final { scan-assembler-times {csrwi\s+vxrm,\s*2} 1 } } */
