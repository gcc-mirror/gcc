/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -fno-schedule-insns -fno-schedule-insns2 -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"


/*
** foo1:
**	flw\t[a-x0-9]+,0\([a-x0-9]+\)
**	ret
*/
float foo1 (void *base, size_t vl)
{
  vfloat32m1_t v = *(vfloat32m1_t*)base;
  float scalar = __riscv_vfmv_f_s_f32m1_f32 (v);
  return scalar;
}

/*
** foo2:
**	flw\t[a-x0-9]+,0\([a-x0-9]+\)
**	ret
*/
float foo2 (void *base, size_t vl)
{
  vfloat32mf2_t v = *(vfloat32mf2_t*)base;
  float scalar = __riscv_vfmv_f_s_f32mf2_f32 (v);
  return scalar;
}

/*
** foo3:
**	flw\t[a-x0-9]+,4\([a-x0-9]+\)
**	ret
*/
float foo3 (float *base, size_t vl)
{
  vfloat32m1_t v = *(vfloat32m1_t*)(base+1);
  float scalar = __riscv_vfmv_f_s_f32m1_f32 (v);
  return scalar;
}

/*
** foo4:
**	lw\t[a-x0-9]+,4\([a-x0-9]+\)
**	ret
*/
int32_t foo4 (float *base, size_t vl)
{
  vfloat32m1_t v = *(vfloat32m1_t*)(base+1);
  float scalar = __riscv_vfmv_f_s_f32m1_f32 (v);
  return *(int32_t*)&scalar;
}
