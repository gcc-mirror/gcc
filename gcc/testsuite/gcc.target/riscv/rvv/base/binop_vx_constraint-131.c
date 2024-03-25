/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */
#include "riscv_vector.h"

/*
** f1:
**  ...
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vmseq\.vi\tv[0-9]+,\s*v[0-9]+,\s*15
**	vmseq\.vi\tv[0-9]+,\s*v[0-9]+,\s*15,v0.t
**	vsm\.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f1 (void * in, void * in2, void *out, int32_t x)
{
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1 (in2, 4);
    vbool32_t m3 = __riscv_vmseq_vx_i32m1_b32 (v, 15, 4);
    vbool32_t m4 = __riscv_vmseq_vx_i32m1_b32_mu (m3, m3, v2, 15, 4);
    __riscv_vsm_v_b32 (out, m4, 4);
}

/*
** f2:
**  ...
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**	vle32.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle32.v\tv[0-9]+,0\([a-x0-9]+\),v0.t
**	vmseq\.vi\tv[0-9]+,\s*v[0-9]+,\s*15
**	vmseq\.vi\tv[1-9][0-9]?,\s*v[0-9]+,\s*15,\s*v0.t
**	vsm.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f2 (void * in, void *out, int32_t x)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_m (mask, in, 4);
    vbool32_t m3 = __riscv_vmseq_vx_i32m1_b32 (v, 15, 4);
    vbool32_t m4 = __riscv_vmseq_vx_i32m1_b32_mu (mask, m3, v2, 15, 4);
    __riscv_vsm_v_b32 (out, m4, 4);
}

/*
** f3:
**  ...
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\),v0.t
**	vmseq\.vi\tv[0-9]+,\s*v[0-9]+,\s*15
**	vmseq\.vi\tv[0-9]+,\s*v[0-9]+,\s*15,\s*v0.t
**	vsm.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f3 (void * in, void *out, int32_t x)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_m (mask, in, 4);
    vbool32_t m3 = __riscv_vmseq_vx_i32m1_b32 (v, 15, 4);
    vbool32_t m4 = __riscv_vmseq_vx_i32m1_b32_m (m3, v2, 15, 4);
    __riscv_vsm_v_b32 (out, m4, 4);
}
