/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwsub_wx_i16mf4(vint16mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i16mf4(op1,0xAA,31);
}


vint16mf2_t test___riscv_vwsub_wx_i16mf2(vint16mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i16mf2(op1,0xAA,31);
}


vint16m1_t test___riscv_vwsub_wx_i16m1(vint16m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i16m1(op1,0xAA,31);
}


vint16m2_t test___riscv_vwsub_wx_i16m2(vint16m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i16m2(op1,0xAA,31);
}


vint16m4_t test___riscv_vwsub_wx_i16m4(vint16m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i16m4(op1,0xAA,31);
}


vint16m8_t test___riscv_vwsub_wx_i16m8(vint16m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i16m8(op1,0xAA,31);
}


vint32mf2_t test___riscv_vwsub_wx_i32mf2(vint32mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i32mf2(op1,0xAA,31);
}


vint32m1_t test___riscv_vwsub_wx_i32m1(vint32m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i32m1(op1,0xAA,31);
}


vint32m2_t test___riscv_vwsub_wx_i32m2(vint32m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i32m2(op1,0xAA,31);
}


vint32m4_t test___riscv_vwsub_wx_i32m4(vint32m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i32m4(op1,0xAA,31);
}


vint32m8_t test___riscv_vwsub_wx_i32m8(vint32m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i32m8(op1,0xAA,31);
}


vint64m1_t test___riscv_vwsub_wx_i64m1(vint64m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i64m1(op1,0xAA,31);
}


vint64m2_t test___riscv_vwsub_wx_i64m2(vint64m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i64m2(op1,0xAA,31);
}


vint64m4_t test___riscv_vwsub_wx_i64m4(vint64m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i64m4(op1,0xAA,31);
}


vint64m8_t test___riscv_vwsub_wx_i64m8(vint64m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwsub_wx_i64m8(op1,0xAA,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwsub\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
