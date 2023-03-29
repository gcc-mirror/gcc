/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

/*
** foo1:
**	lw\t[a-x0-9]+,4\([a-x0-9]+\)
**	lw\t[a-x0-9]+,0\([a-x0-9]+\)
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
**	lw\t[a-x0-9]+,12\([a-x0-9]+\)
**	lw\t[a-x0-9]+,8\([a-x0-9]+\)
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
** ...
** vsrl.vx\tv[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+
** vmv.x.s\t[a-x0-9]+,\s*v[0-9]+
** vmv.x.s\t[a-x0-9]+,\s*v[0-9]+
** ret
*/
int64_t foo3 (void *base, size_t vl)
{
    vint64m1_t v = *(vint64m1_t*)base;
    v = __riscv_vadd_vv_i64m1 (v,v,vl);
    int64_t scalar = __riscv_vmv_x_s_i64m1_i64 (v);
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
