/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** f1:
**	vsetivli\tzero,4,e32,m1,tu,ma
**	vlse32\.v\tv[0-9]+,0\([a-x0-9]+\),\s*zero
**	vlse32\.v\tv[0-9]+,0\([a-x0-9]+\),\s*zero
**	vsse32\.v\tv[0-9]+,0\([a-x0-9]+\),\s*zero
**	ret
*/
void f1 (float * in, float *out)
{
    vfloat32m1_t v = __riscv_vlse32_v_f32m1 (in, 0, 4);
    vfloat32m1_t v2 = __riscv_vlse32_v_f32m1_tu (v, in, 0, 4);
    __riscv_vsse32_v_f32m1 (out, 0, v2, 4);
}

/*
** f2:
**	vsetvli\t[a-x0-9]+,zero,e8,mf4,ta,ma
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**  li\t[a-x0-9]+,\s*7
**	vsetivli\tzero,4,e32,m1,ta,ma
**	vlse32.v\tv[0-9]+,0\([a-x0-9]+\),\s*[a-x0-9]+,v0.t
**	vsse32.v\tv[0-9]+,0\([a-x0-9]+\),\s*[a-x0-9]+
**	ret
*/
void f2 (float * in, float *out)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vfloat32m1_t v = __riscv_vlse32_v_f32m1 (in, 7, 4);
    vfloat32m1_t v2 = __riscv_vlse32_v_f32m1_m (mask, in, 7, 4);
    __riscv_vsse32_v_f32m1 (out, 7, v2, 4);
}


/*
** f3:
**	vsetvli\t[a-x0-9]+,zero,e8,mf4,ta,ma
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**	vsetivli\tzero,4,e32,m1,tu,mu
**	vlse32\.v\tv[0-9]+,0\([a-x0-9]+\),zero
**	vlse32\.v\tv[0-9]+,0\([a-x0-9]+\),zero,v0.t
**	vsse32\.v\tv[0-9]+,0\([a-x0-9]+\),zero
**	ret
*/
void f3 (float * in, float *out)
{
    vbool32_t mask = *(vbool32_t*)in;
    asm volatile ("":::"memory");
    vfloat32m1_t v = __riscv_vlse32_v_f32m1 (in, 0, 4);
    vfloat32m1_t v2 = __riscv_vlse32_v_f32m1_tumu (mask, v, in, 0, 4);
    __riscv_vsse32_v_f32m1 (out, 0, v2, 4);
}

/*
** f4:
**  li\s+[a-x0-9]+,\s*17
**	vsetivli\tzero,4,e8,mf8,tu,ma
**	vlse8\.v\tv[0-9]+,0\([a-x0-9]+\),[a-x0-9]+
**	vlse8\.v\tv[0-9]+,0\([a-x0-9]+\),[a-x0-9]+
**	vsse8\.v\tv[0-9]+,0\([a-x0-9]+\),[a-x0-9]+
**	ret
*/
void f4 (int8_t * in, int8_t *out)
{
    vint8mf8_t v = __riscv_vlse8_v_i8mf8 (in, 17, 4);
    vint8mf8_t v2 = __riscv_vlse8_v_i8mf8_tu (v, in, 17, 4);
    __riscv_vsse8_v_i8mf8 (out, 17, v2, 4);
}

/*
** f5:
**	vsetvli\t[a-x0-9]+,zero,e8,mf8,ta,ma
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**	vsetivli\tzero,4,e8,mf8,ta,ma
**	vlse8.v\tv[0-9]+,0\([a-x0-9]+\),zero,v0.t
**	vsse8.v\tv[0-9]+,0\([a-x0-9]+\),zero
**	ret
*/
void f5 (int8_t * in, int8_t *out)
{
    vbool64_t mask = *(vbool64_t*)in;
    asm volatile ("":::"memory");
    vint8mf8_t v = __riscv_vlse8_v_i8mf8 (in, 0, 4);
    vint8mf8_t v2 = __riscv_vlse8_v_i8mf8_m (mask, in, 0, 4);
    __riscv_vsse8_v_i8mf8 (out, 0, v2, 4);
}

/*
** f6:
**	vsetvli\t[a-x0-9]+,zero,e8,mf8,ta,ma
**	vlm.v\tv[0-9]+,0\([a-x0-9]+\)
**  li\s+[a-x0-9]+,\s*999
**	vsetivli\tzero,4,e8,mf8,tu,mu
**	vlse8\.v\tv[0-9]+,0\([a-x0-9]+\),\s*[a-x0-9]+
**	vlse8\.v\tv[0-9]+,0\([a-x0-9]+\),\s*[a-x0-9]+,v0.t
**	vsse8\.v\tv[0-9]+,0\([a-x0-9]+\),\s*[a-x0-9]+,v0.t
**	ret
*/
void f6 (int8_t * in, int8_t *out)
{
    vbool64_t mask = *(vbool64_t*)in;
    asm volatile ("":::"memory");
    vint8mf8_t v = __riscv_vlse8_v_i8mf8 (in, 999, 4);
    vint8mf8_t v2 = __riscv_vlse8_v_i8mf8_tumu (mask, v, in, 999, 4);
    __riscv_vsse8_v_i8mf8_m (mask,out, 999, v2, 4);
}
