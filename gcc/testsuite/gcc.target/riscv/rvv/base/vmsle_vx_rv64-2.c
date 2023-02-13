/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool64_t test___riscv_vmsle_vx_i8mf8_b64_m(vbool64_t mask,vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i8mf8_b64(op1,op2,31);
}


vbool32_t test___riscv_vmsle_vx_i8mf4_b32_m(vbool32_t mask,vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i8mf4_b32(op1,op2,31);
}


vbool16_t test___riscv_vmsle_vx_i8mf2_b16_m(vbool16_t mask,vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i8mf2_b16(op1,op2,31);
}


vbool8_t test___riscv_vmsle_vx_i8m1_b8_m(vbool8_t mask,vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i8m1_b8(op1,op2,31);
}


vbool4_t test___riscv_vmsle_vx_i8m2_b4_m(vbool4_t mask,vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i8m2_b4(op1,op2,31);
}


vbool2_t test___riscv_vmsle_vx_i8m4_b2_m(vbool2_t mask,vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i8m4_b2(op1,op2,31);
}


vbool1_t test___riscv_vmsle_vx_i8m8_b1_m(vbool1_t mask,vint8m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i8m8_b1(op1,op2,31);
}


vbool64_t test___riscv_vmsle_vx_i16mf4_b64_m(vbool64_t mask,vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i16mf4_b64(op1,op2,31);
}


vbool32_t test___riscv_vmsle_vx_i16mf2_b32_m(vbool32_t mask,vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i16mf2_b32(op1,op2,31);
}


vbool16_t test___riscv_vmsle_vx_i16m1_b16_m(vbool16_t mask,vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i16m1_b16(op1,op2,31);
}


vbool8_t test___riscv_vmsle_vx_i16m2_b8_m(vbool8_t mask,vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i16m2_b8(op1,op2,31);
}


vbool4_t test___riscv_vmsle_vx_i16m4_b4_m(vbool4_t mask,vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i16m4_b4(op1,op2,31);
}


vbool2_t test___riscv_vmsle_vx_i16m8_b2_m(vbool2_t mask,vint16m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i16m8_b2(op1,op2,31);
}


vbool64_t test___riscv_vmsle_vx_i32mf2_b64_m(vbool64_t mask,vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i32mf2_b64(op1,op2,31);
}


vbool32_t test___riscv_vmsle_vx_i32m1_b32_m(vbool32_t mask,vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i32m1_b32(op1,op2,31);
}


vbool16_t test___riscv_vmsle_vx_i32m2_b16_m(vbool16_t mask,vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i32m2_b16(op1,op2,31);
}


vbool8_t test___riscv_vmsle_vx_i32m4_b8_m(vbool8_t mask,vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i32m4_b8(op1,op2,31);
}


vbool4_t test___riscv_vmsle_vx_i32m8_b4_m(vbool4_t mask,vint32m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i32m8_b4(op1,op2,31);
}


vbool64_t test___riscv_vmsle_vx_i64m1_b64_m(vbool64_t mask,vint64m1_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i64m1_b64(op1,op2,31);
}


vbool32_t test___riscv_vmsle_vx_i64m2_b32_m(vbool32_t mask,vint64m2_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i64m2_b32(op1,op2,31);
}


vbool16_t test___riscv_vmsle_vx_i64m4_b16_m(vbool16_t mask,vint64m4_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i64m4_b16(op1,op2,31);
}


vbool8_t test___riscv_vmsle_vx_i64m8_b8_m(vbool8_t mask,vint64m8_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmsle_vx_i64m8_b8(op1,op2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
