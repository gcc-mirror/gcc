/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */
#include "riscv_vector.h"

/*
** f1:
**  ...
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0
**	vse32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f1 (void * in, void *out)
{
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_tu (v, in, 4);
    vint32m1_t v3 = __riscv_vneg_v_i32m1 (v2, 4);
    vint32m1_t v4 = __riscv_vneg_v_i32m1_tu (v3, v2, 4);
    __riscv_vse32_v_i32m1 (out, v4, 4);
}

/*
** f2:
**  ...
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**	vle32.v\tv[0-9]+,0\([a-x0-9]+\),v0.t
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0,\s*v0.t
**	vse32.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f2 (void * in, void *out)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_m (mask, in, 4);
    vint32m1_t v3 = __riscv_vneg_v_i32m1 (v2, 4);
    vint32m1_t v4 = __riscv_vneg_v_i32m1_m (mask, v3, 4);
    __riscv_vse32_v_i32m1 (out, v4, 4);
}

/*
** f3:
**  ...
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**	vle32\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle32.v\tv[0-9]+,0\([a-x0-9]+\),v0.t
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0,\s*v0.t
**	vse32.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f3 (void * in, void *out)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_tumu (mask, v, in, 4);
    vint32m1_t v3 = __riscv_vneg_v_i32m1 (v2, 4);
    vint32m1_t v4 = __riscv_vneg_v_i32m1_tumu (mask, v3, v2, 4);
    __riscv_vse32_v_i32m1 (out, v4, 4);
}

/*
** f4:
**  ...
**	vle8\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle8\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0
**	vse8\.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f4 (void * in, void *out)
{
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in, 4);
    vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tu (v, in, 4);
    vint8mf8_t v3 = __riscv_vneg_v_i8mf8 (v2, 4);
    vint8mf8_t v4 = __riscv_vneg_v_i8mf8_tu (v3, v2, 4);
    __riscv_vse8_v_i8mf8 (out, v4, 4);
}

/*
** f5:
**  ...
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**	vle8.v\tv[0-9]+,0\([a-x0-9]+\),v0.t
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0,\s*v0.t
**	vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f5 (void * in, void *out)
{
    vbool64_t mask = *(vbool64_t*)in;
    asm volatile ("":::"memory");
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in, 4);
    vint8mf8_t v2 = __riscv_vle8_v_i8mf8_m (mask, in, 4);
    vint8mf8_t v3 = __riscv_vneg_v_i8mf8 (v2, 4);
    vint8mf8_t v4 = __riscv_vneg_v_i8mf8_m (mask, v3, 4);
    __riscv_vse8_v_i8mf8 (out, v4, 4);
}

/*
** f6:
**  ...
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**  ...
**	vle8\.v\tv[0-9]+,0\([a-x0-9]+\)
**	vle8.v\tv[0-9]+,0\([a-x0-9]+\),v0.t
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0
**	vrsub\.vi\tv[0-9]+,\s*v[0-9]+,\s*0,\s*v0.t
**	vse8.v\tv[0-9]+,0\([a-x0-9]+\)
**	ret
*/
void f6 (void * in, void *out)
{
    vbool64_t mask = *(vbool64_t*)in;
    asm volatile ("":::"memory");
    vint8mf8_t v = __riscv_vle8_v_i8mf8 (in, 4);
    vint8mf8_t v2 = __riscv_vle8_v_i8mf8_tumu (mask, v, in, 4);
    vint8mf8_t v3 = __riscv_vneg_v_i8mf8 (v2, 4);
    vint8mf8_t v4 = __riscv_vneg_v_i8mf8_tumu (mask, v3, v2, 4);
    __riscv_vse8_v_i8mf8 (out, v4, 4);
}
