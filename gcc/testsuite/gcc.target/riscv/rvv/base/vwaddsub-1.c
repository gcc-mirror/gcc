/* { dg-do compile } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-std=gnu99 -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include <stdint.h>
#include <riscv_vector.h>

/*
** vwadd_wx_i64m8_m:
**    vsetvli\s+zero,[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]
**    vwadd\.wx\tv8,v8,a0,v0.t
**    ret
*/
vint64m8_t
vwadd_wx_i64m8_m (vbool8_t vm, vint64m8_t vs2, int64_t rs1, size_t vl)
{
  return __riscv_vwadd_wx_i64m8_m (vm, vs2, rs1, vl);
}

/*
** vwsub_wx_i64m8_m:
**    vsetvli\s+zero,[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]
**    vwsub\.wx\tv8,v8,a0,v0.t
**    ret
*/
vint64m8_t
vwsub_wx_i64m8_m (vbool8_t vm, vint64m8_t vs2, int64_t rs1, size_t vl)
{
  return __riscv_vwsub_wx_i64m8_m (vm, vs2, rs1, vl);
}

/*
** vwadd_wx_i32m8_m:
**    ...
**    vsetvli\s+zero,[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]
**    vwadd\.wx\tv8,v8,a5,v0.t
**    ret
*/

extern int8_t bla;

vint32m8_t
vwadd_wx_i32m8_m (vbool4_t vm, vint32m8_t vs2, int16_t rs1, size_t vl)
{
  return __riscv_vwadd_wx_i32m8_m (vm, vs2, bla, vl);
}

/* { dg-final { check-function-bodies "**" "" } } */
