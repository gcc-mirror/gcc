/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

/*
** foo1:
**	ld\t[a-x0-9]+,0\([a-x0-9]+\)
**	ret
*/
int64_t foo (void *base, size_t vl)
{
    vint64m1_t v = *(vint64m1_t*)base;
    int64_t scalar = __riscv_vmv_x_s_i64m1_i64 (v);
    return scalar;
}

/*
** foo2:
**	ld\t[a-x0-9]+,8\([a-x0-9]+\)
**	ret
*/
int64_t foo2 (int64_t *base, size_t vl)
{
    vint64m1_t v = *(vint64m1_t*)(base+1);
    int64_t scalar = __riscv_vmv_x_s_i64m1_i64 (v);
    return scalar;
}

/*
** foo3:
** vl1re64\.v\tv[0-9]+,0\([a-x0-9]+\)
** vsetvli\tzero,[a-x0-9]+,e64,m1,t[au],m[au]
** vadd.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
** vsetvli\tzero,[a-x0-9]+,e64,m2,t[au],m[au]
** vmv.x.s\t[a-x0-9]+,\s*v[0-9]+
** vmv.v.x\tv[0-9]+,[a-x0-9]+
** vmv.x.s\t[a-x0-9]+,\s*v[0-9]+
** ret
*/
int64_t foo3 (void *base, size_t vl)
{
    vint64m1_t v = *(vint64m1_t*)base;
    v = __riscv_vadd_vv_i64m1 (v,v,vl);

    int64_t scalar = __riscv_vmv_x_s_i64m1_i64 (v);
    vint64m2_t new_v = __riscv_vmv_v_x_i64m2 (scalar, vl);
    scalar = __riscv_vmv_x_s_i64m2_i64 (new_v);
    return scalar;
}

/*
** foo4:
**	fld\t[a-x0-9]+,4\([a-x0-9]+\)
**	ret
*/
double foo4 (int64_t *base, size_t vl)
{
    vint64m1_t v = *(vint64m1_t*)(base+1);
    int64_t scalar = __riscv_vmv_x_s_i64m1_i64 (v);
    return *(double*)&scalar;
}
