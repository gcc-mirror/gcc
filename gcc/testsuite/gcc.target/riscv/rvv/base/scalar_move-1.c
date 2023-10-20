/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -fno-schedule-insns -fno-schedule-insns2 -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"


/*
** foo1:
**	lw\t[a-x0-9]+,0\([a-x0-9]+\)
**	ret
*/
int32_t foo1 (void *base, size_t vl)
{
  vint32m1_t v = *(vint32m1_t*)base;
  int32_t scalar = __riscv_vmv_x_s_i32m1_i32 (v);
  return scalar;
}

/*
** foo2:
**	lw\t[a-x0-9]+,0\([a-x0-9]+\)
**	ret
*/
int32_t foo2 (void *base, size_t vl)
{
  vint32mf2_t v = *(vint32mf2_t*)base;
  int32_t scalar = __riscv_vmv_x_s_i32mf2_i32 (v);
  return scalar;
}

/*
** foo3:
**	lw\t[a-x0-9]+,4\([a-x0-9]+\)
**	ret
*/
int32_t foo3 (int32_t *base, size_t vl)
{
  vint32m1_t v = *(vint32m1_t*)(base+1);
  int32_t scalar = __riscv_vmv_x_s_i32m1_i32 (v);
  return scalar;
}

/*
** foo4:
** vl1re32\.v\tv[0-9]+,0\([a-x0-9]+\)
** vsetvli\tzero,[a-x0-9]+,e32,m1,t[au],m[au]
** vadd.vv\tv[0-9]+,\s*v[0-9]+,\s*v[0-9]+
** vmv.x.s\t[a-x0-9]+,\s*v[0-9]+
** vsetvli\tzero,[a-x0-9]+,e32,m2,t[au],m[au]
** vmv.v.x\tv[0-9]+,\s*[a-x0-9]+
** vmv.x.s\t[a-x0-9]+,\s*v[0-9]+
** ret
*/
int32_t foo4 (void *base, size_t vl)
{
  vint32m1_t v = *(vint32m1_t*)base;
  v = __riscv_vadd_vv_i32m1 (v,v,vl);
  int32_t scalar = __riscv_vmv_x_s_i32m1_i32 (v);
  vint32m2_t new_v = __riscv_vmv_v_x_i32m2 (scalar, vl);
  scalar = __riscv_vmv_x_s_i32m2_i32 (new_v);
  return scalar;
}

/*
** foo5:
**	flw\t[a-x0-9]+,4\([a-x0-9]+\)
**	ret
*/
float foo5 (int32_t *base, size_t vl)
{
  vint32m1_t v = *(vint32m1_t*)(base+1);
  int32_t scalar = __riscv_vmv_x_s_i32m1_i32 (v);
  return *(float*)&scalar;
}
