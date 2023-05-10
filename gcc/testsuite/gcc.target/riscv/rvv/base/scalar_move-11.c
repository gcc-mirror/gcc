/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O0" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** foo:
** ...
** vsetivli\tzero,0,e64,m4,t[au],m[au]
** vmv.x.s\t[a-x0-9]+,v[0-9]+
** vsetivli\tzero,0,e64,m4,t[au],m[au]
** vmv.x.s\t[a-x0-9]+,v[0-9]+
** ...
*/
int16_t foo (void *base, size_t vl)
{
    int16_t maxVal = __riscv_vmv_x_s_i64m4_i64 (__riscv_vle64_v_i64m4 (base, vl));
    return maxVal;
}
