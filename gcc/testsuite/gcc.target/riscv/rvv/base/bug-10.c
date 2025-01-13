/* { dg-do compile { target { rv64 } } } */
/* { dg-options " -march=rv64gcv_zvfh -mabi=lp64d -O2 --param=vsetvl-strategy=optim -fno-schedule-insns  -fno-schedule-insns2 -fno-schedule-fusion " } */

#include <riscv_vector.h>

void
foo (uint8_t *ptr, vfloat16m4_t *v1, vuint32m8_t *v2, vuint8m2_t *v3, size_t vl)
{
  *v1 = __riscv_vfmv_s_f_f16m4 (1, vl);
  *v2 = __riscv_vmv_s_x_u32m8 (2963090659u, vl);
  *v3 = __riscv_vsll_vx_u8m2 (__riscv_vid_v_u8m2 (vl), 2, vl);
}

/* { dg-final { scan-assembler-not {vsetvli.*zero,zero} } }*/
