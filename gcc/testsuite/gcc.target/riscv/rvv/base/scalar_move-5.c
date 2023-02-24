/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -fno-schedule-insns -fno-schedule-insns2 -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** foo:
** addi\t[a-x0-9]+,\s*[a-x0-9]+,100
** vsetvli\tzero,a2,e64,m2,t[au],m[au]
** vlse64.v\tv[0-9]+,0\([a-x0-9]+\),zero
** vs2r.v\tv[0-9]+,0\([a-x0-9]+\)
** ret
*/
void foo (void *base, void *out, size_t vl)
{
    int64_t scalar = *(int64_t*)(base + 100);
    vint64m2_t v = __riscv_vmv_s_x_i64m2 (scalar, vl);
    *(vint64m2_t*)out = v;
}

/*
** foo2:
** addi\t[a-x0-9]+,\s*[a-x0-9]+,100
** vsetvli\tzero,a2,e64,m2,t[au],m[au]
** vlse64.v\tv[0-9]+,0\([a-x0-9]+\),zero
** vs2r.v\tv[0-9]+,0\([a-x0-9]+\)
** ret
*/
void foo2 (void *base, void *out, size_t vl)
{
    double scalar = *(double*)(base + 100);
    vfloat64m2_t v = __riscv_vfmv_s_f_f64m2 (scalar, vl);
    *(vfloat64m2_t*)out = v;
}

/*
** foo3:
** ...
** vmv.s.x\tv[0-9]+,\s*[a-x0-9]+
** ...
** ret
*/
void foo3 (void *base, void *out, size_t vl)
{
    int64_t scalar = *(int64_t*)(base + 100);
    vint64m2_t merge = *(vint64m2_t*) (base + 200);
    vint64m2_t v = __riscv_vmv_s_x_i64m2_tu (merge, scalar, vl);
    *(vint64m2_t*)out = v;
}

/*
** foo4:
** ...
** vfmv.s.f\tv[0-9]+,\s*[a-x0-9]+
** ...
** ret
*/
void foo4 (void *base, void *out, size_t vl)
{
    double scalar = *(double*)(base + 100);
    vfloat64m2_t merge = *(vfloat64m2_t*) (base + 200);
    vfloat64m2_t v = __riscv_vfmv_s_f_f64m2_tu (merge, scalar, vl);
    *(vfloat64m2_t*)out = v;
}

/*
** foo5:
** ...
** vmv.s.x\tv[0-9]+,\s*[a-x0-9]+
** ...
** ret
*/
void foo5 (void *base, void *out, size_t vl, int64_t x)
{
    vint64m2_t v = __riscv_vmv_s_x_i64m2 (x, vl);
    *(vint64m2_t*)out = v;
}

/*
** foo6:
** ...
** vfmv.s.f\tv[0-9]+,\s*[a-x0-9]+
** ...
** ret
*/
void foo6 (void *base, void *out, size_t vl, double x)
{
    vfloat64m2_t v = __riscv_vfmv_s_f_f64m2 (x, vl);
    *(vfloat64m2_t*)out = v;
}

/*
** foo7:
** ...
** vmv.s.x\tv[0-9]+,\s*[a-x0-9]+
** ...
** ret
*/
void foo7 (void *base, void *out, size_t vl, int64_t x)
{
    vint64m2_t merge = *(vint64m2_t*) (base + 200);
    vint64m2_t v = __riscv_vmv_s_x_i64m2_tu (merge, x, vl);
    *(vint64m2_t*)out = v;
}

/*
** foo8:
** ...
** vfmv.s.f\tv[0-9]+,\s*[a-x0-9]+
** ...
** ret
*/
void foo8 (void *base, void *out, size_t vl, double x)
{
    vfloat64m2_t merge = *(vfloat64m2_t*) (base + 200);
    vfloat64m2_t v = __riscv_vfmv_s_f_f64m2_tu (merge, x, vl);
    *(vfloat64m2_t*)out = v;
}

/*
** foo9:
** ...
** vmv.v.i\tv[0-9]+,\s*-15
** ...
** ret
*/
void foo9 (void *base, void *out, size_t vl)
{
    int64_t scalar = *(int64_t*)(base + 100);
    vint64m2_t v = __riscv_vmv_s_x_i64m2 (-15, vl);
    *(vint64m2_t*)out = v;
}

/*
** foo10:
** ...
** vmv.s.x\tv[0-9]+,\s*[a-x0-9]+
** ...
** ret
*/
void foo10 (void *base, void *out, size_t vl)
{
    int64_t scalar = *(int64_t*)(base + 100);
    vint64m2_t merge = *(vint64m2_t*) (base + 200);
    vint64m2_t v = __riscv_vmv_s_x_i64m2_tu (merge, -15, vl);
    *(vint64m2_t*)out = v;
}

/*
** foo11:
** ...
** vmv.v.i\tv[0-9]+,\s*0
** ...
** ret
*/
void foo11 (void *base, void *out, size_t vl)
{
    double scalar = *(double*)(base + 100);
    vfloat64m2_t v = __riscv_vfmv_s_f_f64m2 (0, vl);
    *(vfloat64m2_t*)out = v;
}

/*
** foo12:
** ...
** vfmv.s.f\tv[0-9]+,\s*[a-x0-9]+
** ...
** ret
*/
void foo12 (void *base, void *out, size_t vl)
{
    vfloat64m2_t merge = *(vfloat64m2_t*) (base + 200);
    vfloat64m2_t v = __riscv_vfmv_s_f_f64m2_tu (merge, 0, vl);
    *(vfloat64m2_t*)out = v;
}
