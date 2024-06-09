/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */
#include "riscv_vector.h"

/*
** f1:
**  ...
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+
**	vse32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f1 (void * in, void * in2, void *out, int32_t x)
{
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1 (in2, 4);
    vint32m1_t v3 = __riscv_vmacc_vx_i32m1 (v, x, v2, 4);
    vint32m1_t v4 = __riscv_vmacc_vx_i32m1_tu (v3, x, v2, 4);
    __riscv_vse32_v_i32m1 (out, v4, 4);
}

/*
** f2:
**  ...
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**	vle32.v\tv[0-9]+,0\([a-x0-9]+\),v0.t
**	vle32.v\tv[0-9]+,0\([a-x0-9]+\)
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+
**	vse32.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f2 (void * in, void * in2, void *out, int32_t x)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_m (mask, in2, 4);
    vint32m1_t v3 = __riscv_vmacc_vx_i32m1 (v, x, v2, 4);
    vint32m1_t v4 = __riscv_vmacc_vx_i32m1_tu (v3, x, v2, 4);
    __riscv_vse32_v_i32m1 (out, v4, 4);
}

/*
** f3:
**  ...
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**	vle32.v\tv[0-9]+,0\([a-x0-9]+\),v0.t
**	vle32.v\tv[0-9]+,0\([a-x0-9]+\)
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+
**	vma[c-d][c-d]\.vx\tv[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,v0.t
**	vse32.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f3 (void * in, void * in2, void *out, int32_t x)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_m (mask, in2, 4);
    vint32m1_t v3 = __riscv_vmacc_vx_i32m1 (v, x, v2, 4);
    vint32m1_t v4 = __riscv_vmacc_vx_i32m1_tumu (mask, v3, x, v2, 4);
    __riscv_vse32_v_i32m1 (out, v4, 4);
}

/* { dg-final { scan-assembler-not {vmv} } } */
