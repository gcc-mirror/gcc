/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */
#include "riscv_vector.h"

void f1 (void * in, void *out, int32_t x)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_m (mask, in, 4);
    vbool32_t m3 = __riscv_vmseq_vx_i32m1_b32 (v, x, 4);
    vbool32_t m4 = __riscv_vmseq_vx_i32m1_b32_mu (mask, m3, v, x, 4);
    m4 = __riscv_vmseq_vv_i32m1_b32_m (m4, v2, v2, 4);
    __riscv_vsm_v_b32 (out, m4, 4);
}

void f2 (void * in, void *out, int32_t x)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_m (mask, in, 4);
    vbool32_t m3 = __riscv_vmslt_vx_i32m1_b32 (v, x, 4);
    vbool32_t m4 = __riscv_vmslt_vx_i32m1_b32_mu (mask, m3, v, x, 4);
    m4 = __riscv_vmslt_vv_i32m1_b32_m (m4, v2, v2, 4);
    __riscv_vsm_v_b32 (out, m4, 4);
}

/* { dg-final { scan-assembler-times {vmv} 2 } } */
