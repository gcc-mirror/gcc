/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh_zfh -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (_Float16 *pSrcA, _Float16 *pSrcB, uint32_t n, double *result)
{
  size_t vl;
  vfloat16m4_t vSrcA, vSrcB;
  vfloat64m1_t vSum = __riscv_vfmv_s_f_f64m1 (0, 1);
  while (n > 0)
    {
      vl = __riscv_vsetvl_e16m4 (n);
      vSrcA = __riscv_vle16_v_f16m4 (pSrcA, vl);
      vSrcB = __riscv_vle16_v_f16m4 (pSrcB, vl);
      vSum = __riscv_vfwredusum_vs_f32m8_f64m1 (
	__riscv_vfwmul_vv_f32m8 (vSrcA, vSrcB, vl), vSum, vl);
      pSrcA += vl;
      pSrcB += vl;
      n -= vl;
    }
  *result = __riscv_vfmv_f_s_f64m1_f64 (vSum);
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv\.v\.v} } } */
