/* { dg-do compile } */
/* { dg-options "-march=rv32gcxtheadvector -mabi=ilp32d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */
#include "riscv_th_vector.h"

/*
** f1:
**	li\t[a-x0-9]+,4
**	th.vsetvli\tzero,[a-x0-9]+,e32,m1
**	th.vlhu\.v\tv[0-9]+,0\([a-x0-9]+\)
**	th.vlhu\.v\tv[0-9]+,0\([a-x0-9]+\)
**	th.vadd\.vi\tv[0-9]+,\s*v[0-9]+,\s*-16
**	th.vadd\.vi\tv[0-9]+,\s*v[0-9]+,\s*-16
**	th.vsh\.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f1 (void * in, void *out, uint32_t x)
{
    vuint32m1_t v = __riscv_th_vlhu_v_u32m1 (in, 4);
    vuint32m1_t v2 = __riscv_th_vlhu_v_u32m1_tu (v, in, 4);
    vuint32m1_t v3 = __riscv_vadd_vx_u32m1 (v2, -16, 4);
    vuint32m1_t v4 = __riscv_vadd_vx_u32m1_tu (v3, v2, -16, 4);
    __riscv_th_vsh_v_u32m1 (out, v4, 4);
}

/*
** f2:
**	th.vsetvli\tzero,zero,e8,m1
**	th.vle.v\tv[0-9]+,0\([a-x0-9]+\)
**	li\t[a-x0-9]+,4
**	th.vsetvli\tzero,[a-x0-9]+,e32,m1
**	th.vlhu.v\tv[0-9]+,0\([a-x0-9]+\),v0.t
**	th.vadd\.vi\tv[0-9]+,\s*v[0-9]+,\s*-16
**	th.vadd\.vi\tv[1-9][0-9]?,\s*v[0-9]+,\s*-16,\s*v0.t
**	th.vsh.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f2 (void * in, void *out, uint32_t x)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vuint32m1_t v = __riscv_th_vlhu_v_u32m1 (in, 4);
    vuint32m1_t v2 = __riscv_th_vlhu_v_u32m1_m (mask, in, 4);
    vuint32m1_t v3 = __riscv_vadd_vx_u32m1 (v2, -16, 4);
    vuint32m1_t v4 = __riscv_vadd_vx_u32m1_m (mask, v3, -16, 4);
    __riscv_th_vsh_v_u32m1 (out, v4, 4);
}

/*
** f3:
**	th.vsetvli\tzero,zero,e8,m1
**	th.vle.v\tv[0-9]+,0\([a-x0-9]+\)
**	li\t[a-x0-9]+,4
**	th.vsetvli\tzero,[a-x0-9]+,e32,m1
**	th.vlhu\.v\tv[0-9]+,0\([a-x0-9]+\)
**	th.vlhu.v\tv[0-9]+,0\([a-x0-9]+\),v0.t
**	th.vadd\.vi\tv[0-9]+,\s*v[0-9]+,\s*-16
**	th.vadd\.vi\tv[1-9][0-9]?,\s*v[0-9]+,\s*-16,\s*v0.t
**	th.vsh.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f3 (void * in, void *out, uint32_t x)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vuint32m1_t v = __riscv_th_vlhu_v_u32m1 (in, 4);
    vuint32m1_t v2 = __riscv_th_vlhu_v_u32m1_tumu (mask, v, in, 4);
    vuint32m1_t v3 = __riscv_vadd_vx_u32m1 (v2, -16, 4);
    vuint32m1_t v4 = __riscv_vadd_vx_u32m1_tumu (mask, v3, v2, -16, 4);
    __riscv_th_vsh_v_u32m1 (out, v4, 4);
}
