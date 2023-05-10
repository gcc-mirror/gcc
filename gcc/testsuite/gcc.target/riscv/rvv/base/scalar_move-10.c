/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O0" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** foo1:
** ...
** vsetivli\tzero,0,e16,m1,t[au],m[au]
** vmv.x.s\t[a-x0-9]+,v[0-9]+
** ...
*/
int16_t foo1 (void *base, size_t vl)
{
    int16_t maxVal = __riscv_vmv_x_s_i16m1_i16 (__riscv_vle16_v_i16m1 (base, vl));
    return maxVal;
}

/*
** foo2:
** ...
** vsetivli\tzero,0,e32,m1,t[au],m[au]
** vfmv.f.s\tf[a-x0-9]+,v[0-9]+
** ...
*/
float foo2 (void *base, size_t vl)
{
    float maxVal = __riscv_vfmv_f_s_f32m1_f32 (__riscv_vle32_v_f32m1 (base, vl));
    return maxVal;
}
