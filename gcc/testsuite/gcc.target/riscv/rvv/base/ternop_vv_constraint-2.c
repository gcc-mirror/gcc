/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */
#include "riscv_vector.h"

/*
** f1:
**	vsetivli\tzero,4,e32,m1,ta,ma
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**	vse32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f1 (void * in, void * in2, void *out)
{
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1 (in2, 4);
    vint32m1_t v3 = __riscv_vmadd_vv_i32m1 (v, v2, v2, 4);
    vint32m1_t v4 = __riscv_vmadd_vv_i32m1(v3, v2, v2, 4);
    v4 = __riscv_vmadd_vv_i32m1 (v4, v2, v2, 4);
    v4 = __riscv_vmadd_vv_i32m1 (v4, v2, v2, 4);
    v4 = __riscv_vmadd_vv_i32m1 (v4, v2, v2, 4);
    __riscv_vse32_v_i32m1 (out, v4, 4);
}

/*
** f2:
**	vsetivli\tzero,4,e32,m1,tu,ma
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
**	vse32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f2 (void * in, void * in2, void *out)
{
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1 (in2, 4);
    vint32m1_t v3 = __riscv_vmadd_vv_i32m1_tu (v, v2, v2, 4);
    vint32m1_t v4 = __riscv_vmadd_vv_i32m1_tu(v3, v2, v2, 4);
    v4 = __riscv_vmadd_vv_i32m1_tu (v4, v2, v2, 4);
    v4 = __riscv_vmadd_vv_i32m1_tu (v4, v2, v2, 4);
    v4 = __riscv_vmadd_vv_i32m1_tu (v4, v2, v2, 4);
    __riscv_vse32_v_i32m1 (out, v4, 4);
}

/*
** f3:
**	vsetivli\tzero,4,e32,m1,ta,ma
**	vlm\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,v0.t
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,v0.t
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,v0.t
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,v0.t
**	vma[c-d][c-d]\.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+,v0.t
**	vse32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f3 (void * in, void * in2, void * in3, void *out)
{
    vbool32_t m = __riscv_vlm_v_b32 (in3, 4);
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1 (in2, 4);
    vint32m1_t v3 = __riscv_vmadd_vv_i32m1_m (m, v, v2, v2, 4);
    vint32m1_t v4 = __riscv_vmadd_vv_i32m1_m(m, v3, v2, v2, 4);
    v4 = __riscv_vmadd_vv_i32m1_m (m, v4, v2, v2, 4);
    v4 = __riscv_vmadd_vv_i32m1_m (m, v4, v2, v2, 4);
    v4 = __riscv_vmadd_vv_i32m1_m (m, v4, v2, v2, 4);
    __riscv_vse32_v_i32m1 (out, v4, 4);
}

/* { dg-final { scan-assembler-not {vmv} } } */
