/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** foo9:
**   vsetivli\tzero,1,e64,m2,t[au],m[au]
**   ...
**   vs2r.v\tv[0-9]+,0\([a-x0-9]+\)
**   ret
*/
void foo9 (void *base, void *out, size_t vl)
{
    int64_t scalar = *(int64_t*)(base + 100);
    vint64m2_t v = __riscv_vmv_v_x_i64m2 (0, 1);
    *(vint64m2_t*)out = v;
}

/*
** foo10:
**   vsetivli\tzero,1,e64,m2,t[au],m[au]
**   ...
**   vs2r.v\tv[0-9]+,0\([a-x0-9]+\)
**   ret
*/
void foo10 (void *base, void *out, size_t vl)
{
    int64_t scalar = *(int64_t*)(base + 100);
    vint64m2_t v = __riscv_vmv_s_x_i64m2 (0, 1);
    *(vint64m2_t*)out = v;
}
