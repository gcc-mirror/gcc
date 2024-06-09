/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (int16_t *pSrcA, int16_t *pSrcB, uint32_t n, int64_t *result)
{
  size_t vl;
  vint16m4_t vSrcA, vSrcB;
  vint64m1_t vSum = __riscv_vmv_s_x_i64m1 (0, 1);
  while (n > 0)
    {
      vl = __riscv_vsetvl_e16m4 (n);
      vSrcA = __riscv_vle16_v_i16m4 (pSrcA, vl);
      vSrcB = __riscv_vle16_v_i16m4 (pSrcB, vl);
      vSum = __riscv_vwredsum_vs_i32m8_i64m1 (
	__riscv_vwmul_vv_i32m8 (vSrcA, vSrcB, vl), vSum, vl);
      pSrcA += vl;
      pSrcB += vl;
      n -= vl;
    }
  *result = __riscv_vmv_x_s_i64m1_i64 (vSum);
}

/* { dg-final { scan-assembler-not {vmv1r} } } */
/* { dg-final { scan-assembler-not {vmv\.v\.v} } } */
