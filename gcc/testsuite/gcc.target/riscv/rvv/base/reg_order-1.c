/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void f (void *base,void *base2,void *out,size_t vl, int n)
{
    vuint64m8_t bindex = __riscv_vle64_v_u64m8 (base + 100, vl);
    for (int i = 0; i < n; i++){
      vbool8_t m = __riscv_vlm_v_b8 (base + i, vl);
      vuint64m8_t v = __riscv_vluxei64_v_u64m8_m(m,base,bindex,vl);
      vuint64m8_t v2 = __riscv_vle64_v_u64m8_tu (v, base2 + i, vl);
      vint8m1_t v3 = __riscv_vluxei64_v_i8m1_m(m,base,v,vl);
      vint8m1_t v4 = __riscv_vluxei64_v_i8m1_m(m,base,v2,vl);
      __riscv_vse8_v_i8m1 (out + 100*i,v3,vl);
      __riscv_vse8_v_i8m1 (out + 222*i,v4,vl);
    }
}

/* { dg-final { scan-assembler-not {csrr} } } */
