/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void matrix_transpose_in_register(uint32_t* outputMat, uint32_t* inputMat) {
    vuint32m1_t matRegIn0 = __riscv_vle32_v_u32m1(inputMat, 4);
    vuint32m1_t matRegIn1 = __riscv_vle32_v_u32m1(inputMat + 4, 4);
    vuint32m1_t matRegIn2 = __riscv_vle32_v_u32m1(inputMat + 8, 4);
    vuint32m1_t matRegIn3 = __riscv_vle32_v_u32m1(inputMat + 12, 4);

    vbool32_t oddMask = __riscv_vreinterpret_v_u32m1_b32(__riscv_vmv_v_x_u32m1(0xaaaa, 1));

    vuint32m1_t smallTransposeMat0 = __riscv_vslideup_vx_u32m1_tumu(oddMask, matRegIn0, matRegIn1, 1, 4);
    vuint32m1_t smallTransposeMat2 = __riscv_vslideup_vx_u32m1_tumu(oddMask, matRegIn2, matRegIn3, 1, 4);

    vbool32_t evenMask = __riscv_vreinterpret_v_u32m1_b32(__riscv_vmv_v_x_u32m1(0x5555, 1));

    vuint32m1_t smallTransposeMat1 = __riscv_vslidedown_vx_u32m1_tumu(evenMask, matRegIn1, matRegIn0, 1, 4);
    vuint32m1_t smallTransposeMat3 = __riscv_vslidedown_vx_u32m1_tumu(evenMask, matRegIn3, matRegIn2, 1, 4);

    vuint32m1_t outMat0 = __riscv_vslideup_vx_u32m1_tu(smallTransposeMat0, smallTransposeMat2, 2, 4);
    vuint32m1_t outMat1 = __riscv_vslideup_vx_u32m1_tu(smallTransposeMat1, smallTransposeMat3, 2, 4);

    vuint32m1_t outMat2 = __riscv_vslidedown_vx_u32m1_tu(smallTransposeMat2, smallTransposeMat0, 2, 2);
    vuint32m1_t outMat3 = __riscv_vslidedown_vx_u32m1_tu(smallTransposeMat3, smallTransposeMat1, 2, 2);
    __riscv_vse32_v_u32m1(outputMat, outMat0, 4);
    __riscv_vse32_v_u32m1(outputMat + 4, outMat1, 4);
    __riscv_vse32_v_u32m1(outputMat + 8, outMat2, 4);
    __riscv_vse32_v_u32m1(outputMat + 12, outMat3, 4);
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*4,\s*e32,\s*m1,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*2,\s*e32,\s*m1,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli} 3 } } */
/* { dg-final { scan-assembler-not {vsetvli} } } */
