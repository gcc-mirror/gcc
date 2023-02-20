/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3" } */
#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmulsu_vx_i16mf4(vbool64_t mask, vint16mf4_t merge, vint8mf8_t op1,int8_t op2,size_t vl)
{
  return __riscv_vwmulsu_vx_i16mf4_tumu(mask,merge,op1,0,vl);
}

vint16mf4_t test___riscv_vwmul_vx_i16mf4(vbool64_t mask, vint16mf4_t merge, vint8mf8_t op1,int8_t op2,size_t vl)
{
  return __riscv_vwmul_vx_i16mf4_tumu(mask,merge,op1,0,vl);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,zero} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,zero} 1 } } */
