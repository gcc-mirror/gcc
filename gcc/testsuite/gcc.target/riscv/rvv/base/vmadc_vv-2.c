/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool64_t test___riscv_vmadc_vv_i8mf8_b64(vint8mf8_t op1,vint8mf8_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i8mf8_b64(op1,op2,31);
}


vbool32_t test___riscv_vmadc_vv_i8mf4_b32(vint8mf4_t op1,vint8mf4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i8mf4_b32(op1,op2,31);
}


vbool16_t test___riscv_vmadc_vv_i8mf2_b16(vint8mf2_t op1,vint8mf2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i8mf2_b16(op1,op2,31);
}


vbool8_t test___riscv_vmadc_vv_i8m1_b8(vint8m1_t op1,vint8m1_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i8m1_b8(op1,op2,31);
}


vbool4_t test___riscv_vmadc_vv_i8m2_b4(vint8m2_t op1,vint8m2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i8m2_b4(op1,op2,31);
}


vbool2_t test___riscv_vmadc_vv_i8m4_b2(vint8m4_t op1,vint8m4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i8m4_b2(op1,op2,31);
}


vbool1_t test___riscv_vmadc_vv_i8m8_b1(vint8m8_t op1,vint8m8_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i8m8_b1(op1,op2,31);
}


vbool64_t test___riscv_vmadc_vv_i16mf4_b64(vint16mf4_t op1,vint16mf4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i16mf4_b64(op1,op2,31);
}


vbool32_t test___riscv_vmadc_vv_i16mf2_b32(vint16mf2_t op1,vint16mf2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i16mf2_b32(op1,op2,31);
}


vbool16_t test___riscv_vmadc_vv_i16m1_b16(vint16m1_t op1,vint16m1_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i16m1_b16(op1,op2,31);
}


vbool8_t test___riscv_vmadc_vv_i16m2_b8(vint16m2_t op1,vint16m2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i16m2_b8(op1,op2,31);
}


vbool4_t test___riscv_vmadc_vv_i16m4_b4(vint16m4_t op1,vint16m4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i16m4_b4(op1,op2,31);
}


vbool2_t test___riscv_vmadc_vv_i16m8_b2(vint16m8_t op1,vint16m8_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i16m8_b2(op1,op2,31);
}


vbool64_t test___riscv_vmadc_vv_i32mf2_b64(vint32mf2_t op1,vint32mf2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i32mf2_b64(op1,op2,31);
}


vbool32_t test___riscv_vmadc_vv_i32m1_b32(vint32m1_t op1,vint32m1_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i32m1_b32(op1,op2,31);
}


vbool16_t test___riscv_vmadc_vv_i32m2_b16(vint32m2_t op1,vint32m2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i32m2_b16(op1,op2,31);
}


vbool8_t test___riscv_vmadc_vv_i32m4_b8(vint32m4_t op1,vint32m4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i32m4_b8(op1,op2,31);
}


vbool4_t test___riscv_vmadc_vv_i32m8_b4(vint32m8_t op1,vint32m8_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i32m8_b4(op1,op2,31);
}


vbool64_t test___riscv_vmadc_vv_i64m1_b64(vint64m1_t op1,vint64m1_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i64m1_b64(op1,op2,31);
}


vbool32_t test___riscv_vmadc_vv_i64m2_b32(vint64m2_t op1,vint64m2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i64m2_b32(op1,op2,31);
}


vbool16_t test___riscv_vmadc_vv_i64m4_b16(vint64m4_t op1,vint64m4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i64m4_b16(op1,op2,31);
}


vbool8_t test___riscv_vmadc_vv_i64m8_b8(vint64m8_t op1,vint64m8_t op2,size_t vl)
{
    return __riscv_vmadc_vv_i64m8_b8(op1,op2,31);
}


vbool64_t test___riscv_vmadc_vv_u8mf8_b64(vuint8mf8_t op1,vuint8mf8_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u8mf8_b64(op1,op2,31);
}


vbool32_t test___riscv_vmadc_vv_u8mf4_b32(vuint8mf4_t op1,vuint8mf4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u8mf4_b32(op1,op2,31);
}


vbool16_t test___riscv_vmadc_vv_u8mf2_b16(vuint8mf2_t op1,vuint8mf2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u8mf2_b16(op1,op2,31);
}


vbool8_t test___riscv_vmadc_vv_u8m1_b8(vuint8m1_t op1,vuint8m1_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u8m1_b8(op1,op2,31);
}


vbool4_t test___riscv_vmadc_vv_u8m2_b4(vuint8m2_t op1,vuint8m2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u8m2_b4(op1,op2,31);
}


vbool2_t test___riscv_vmadc_vv_u8m4_b2(vuint8m4_t op1,vuint8m4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u8m4_b2(op1,op2,31);
}


vbool1_t test___riscv_vmadc_vv_u8m8_b1(vuint8m8_t op1,vuint8m8_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u8m8_b1(op1,op2,31);
}


vbool64_t test___riscv_vmadc_vv_u16mf4_b64(vuint16mf4_t op1,vuint16mf4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u16mf4_b64(op1,op2,31);
}


vbool32_t test___riscv_vmadc_vv_u16mf2_b32(vuint16mf2_t op1,vuint16mf2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u16mf2_b32(op1,op2,31);
}


vbool16_t test___riscv_vmadc_vv_u16m1_b16(vuint16m1_t op1,vuint16m1_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u16m1_b16(op1,op2,31);
}


vbool8_t test___riscv_vmadc_vv_u16m2_b8(vuint16m2_t op1,vuint16m2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u16m2_b8(op1,op2,31);
}


vbool4_t test___riscv_vmadc_vv_u16m4_b4(vuint16m4_t op1,vuint16m4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u16m4_b4(op1,op2,31);
}


vbool2_t test___riscv_vmadc_vv_u16m8_b2(vuint16m8_t op1,vuint16m8_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u16m8_b2(op1,op2,31);
}


vbool64_t test___riscv_vmadc_vv_u32mf2_b64(vuint32mf2_t op1,vuint32mf2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u32mf2_b64(op1,op2,31);
}


vbool32_t test___riscv_vmadc_vv_u32m1_b32(vuint32m1_t op1,vuint32m1_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u32m1_b32(op1,op2,31);
}


vbool16_t test___riscv_vmadc_vv_u32m2_b16(vuint32m2_t op1,vuint32m2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u32m2_b16(op1,op2,31);
}


vbool8_t test___riscv_vmadc_vv_u32m4_b8(vuint32m4_t op1,vuint32m4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u32m4_b8(op1,op2,31);
}


vbool4_t test___riscv_vmadc_vv_u32m8_b4(vuint32m8_t op1,vuint32m8_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u32m8_b4(op1,op2,31);
}


vbool64_t test___riscv_vmadc_vv_u64m1_b64(vuint64m1_t op1,vuint64m1_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u64m1_b64(op1,op2,31);
}


vbool32_t test___riscv_vmadc_vv_u64m2_b32(vuint64m2_t op1,vuint64m2_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u64m2_b32(op1,op2,31);
}


vbool16_t test___riscv_vmadc_vv_u64m4_b16(vuint64m4_t op1,vuint64m4_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u64m4_b16(op1,op2,31);
}


vbool8_t test___riscv_vmadc_vv_u64m8_b8(vuint64m8_t op1,vuint64m8_t op2,size_t vl)
{
    return __riscv_vmadc_vv_u64m8_b8(op1,op2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vmadc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
