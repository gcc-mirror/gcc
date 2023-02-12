/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** f1:
** 	vsetivli\s+zero,4,e32,mf2,tu,m[au]
** 	vlse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*zero
** 	vlse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*zero
** 	vluxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+
** 	vsoxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+
** 	ret
*/
void f1 (void * in, void * in2, void *out)
{
  vfloat32mf2_t v = __riscv_vlse32_v_f32mf2 (in, 0, 4);
  vuint8mf8_t index = __riscv_vlse8_v_u8mf8 (in2, 0, 4);
  vfloat32mf2_t v2 = __riscv_vluxei8_v_f32mf2_tu (v, in, index, 4);
  __riscv_vsoxei8_v_f32mf2 (out, index, v2, 4);
}

/*
** f2:
** 	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]
** 	vlm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
** 	vsetivli\s+zero,4,e32,mf2,\s*t[au],\s*m[au]
** 	vlse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*zero
** 	vluxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+,v0.t
** 	vsoxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+
** 	ret
*/
void f2 (void * in, void * in2, void *out)
{
  vbool64_t mask = *(vbool64_t*)in;
  asm volatile ("":::"memory");
  vfloat32mf2_t v = __riscv_vlse32_v_f32mf2 (in, 0, 4);
  vuint8mf8_t index = __riscv_vlse8_v_u8mf8 (in2, 0, 4);
  vfloat32mf2_t v2 = __riscv_vluxei8_v_f32mf2_m (mask, in, index, 4);
  __riscv_vsoxei8_v_f32mf2 (out, index, v2, 4);
}

/*
** f3:
** 	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]
** 	vlm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
** 	vsetivli\s+zero,\s*4,\s*e32,\s*mf2,\s*tu,\s*mu
** 	vlse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*zero
** 	vlse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*zero
** 	vluxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+,v0.t
** 	vsoxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+
** 	ret
*/
void f3 (void * in, void * in2, void *out)
{
  vbool64_t mask = *(vbool64_t*)in;
  asm volatile ("":::"memory");
  vfloat32mf2_t v = __riscv_vlse32_v_f32mf2 (in, 0, 4);
  vuint8mf8_t index = __riscv_vlse8_v_u8mf8 (in2, 0, 4);
  vfloat32mf2_t v2 = __riscv_vluxei8_v_f32mf2_tumu (mask, v, in, index, 4);
  __riscv_vsoxei8_v_f32mf2 (out, index, v2, 4);
}

/*
** f4:
** 	vsetivli\s+zero,4,e8,mf8,tu,\s*m[au]
** 	vlse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),zero
** 	vluxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+
** 	vluxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+
** 	vsoxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+
** 	ret
*/
void f4 (void * in, void * in2, void *out)
{
  vuint8mf8_t index = __riscv_vlse8_v_u8mf8 (in2, 0, 4);
  vint8mf8_t v = __riscv_vluxei8_v_i8mf8 (in, index, 4);
  vint8mf8_t v2 = __riscv_vluxei8_v_i8mf8_tu (v, in, index, 4);
  __riscv_vsoxei8_v_i8mf8 (out, index, v2, 4);
}

/*
** f5:
** 	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]
** 	vlm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
** 	vsetivli\s+zero,4,e8,mf8,t[au],m[au]
** 	vlse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),zero
** 	vluxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+,v0.t
** 	vsoxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+
** 	ret
*/
void f5 (void * in, void * in2, void *out)
{
  vbool64_t mask = *(vbool64_t*)in;
  asm volatile ("":::"memory");
  vuint8mf8_t index = __riscv_vlse8_v_u8mf8 (in2, 0, 4);
  vint8mf8_t v = __riscv_vluxei8_v_i8mf8 (in, index, 4);
  vint8mf8_t v2 = __riscv_vluxei8_v_i8mf8_m (mask, in, index, 4);
  __riscv_vsoxei8_v_i8mf8 (out, index, v2, 4);
}

/*
** f6:
** 	vsetvli\s+[a-x0-9]+,\s*zero,\s*e8,\s*mf8,\s*t[au],\s*m[au]
** 	vlm\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)
** 	vsetivli\s+zero,4,e8,mf8,tu,mu
** 	vlse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),zero
** 	vluxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+
** 	vluxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+,v0.t
** 	vsoxei8\.v\s+v[0-9]+,\s*\([a-x0-9]+\),\s*v[0-9]+
** 	ret
*/
void f6 (void * in, void * in2, void *out)
{
  vbool64_t mask = *(vbool64_t*)in;
  asm volatile ("":::"memory");
  vuint8mf8_t index = __riscv_vlse8_v_u8mf8 (in2, 0, 4);
  vint8mf8_t v = __riscv_vluxei8_v_i8mf8 (in, index, 4);
  vint8mf8_t v2 = __riscv_vluxei8_v_i8mf8_tumu (mask, v, in, index, 4);
  __riscv_vsoxei8_v_i8mf8 (out, index, v2, 4);
}
