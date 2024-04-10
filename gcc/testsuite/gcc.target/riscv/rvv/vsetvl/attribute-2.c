/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (uint32_t *outputMat, uint32_t *inputMat)
{
  vuint32m1_t matRegIn0 = __riscv_vle32_v_u32m1 (inputMat, 4);
  vuint32m1_t matRegIn1 = __riscv_vle32_v_u32m1 (inputMat + 4, 4);
  vuint32m1_t matRegIn2 = __riscv_vle32_v_u32m1 (inputMat + 8, 4);
  vuint32m1_t matRegIn3 = __riscv_vle32_v_u32m1 (inputMat + 12, 4);

  vbool32_t oddMask
    = __riscv_vreinterpret_v_u32m1_b32 (__riscv_vmv_v_x_u32m1 (0xaaaa, 1));

  vuint32m1_t smallTransposeMat0
    = __riscv_vslideup_vx_u32m1_tumu (oddMask, matRegIn0, matRegIn1, 1, 4);
  vuint32m1_t smallTransposeMat2
    = __riscv_vslideup_vx_u32m1_tumu (oddMask, matRegIn2, matRegIn3, 1, 4);

  vuint32m1_t outMat0 = __riscv_vslideup_vx_u32m1_tu (smallTransposeMat0,
						      smallTransposeMat2, 2, 4);

  __riscv_vse32_v_u32m1 (outputMat, outMat0, 4);
}

void
foo2 (void *outputMat, void *inputMat)
{
  vfloat32m1_t v = __riscv_vfmv_v_f_f32m1 (0xaaaa, 1);
  __riscv_vse32_v_f32m1 (outputMat, v, 4);
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*4,\s*e32,\s*m1,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli} 2 } } */
/* { dg-final { scan-assembler-not {vsetvli} } } */
