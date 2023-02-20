/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vadc_vxm_i8mf8(vint8mf8_t op1,int8_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i8mf8(op1,op2,carryin,31);
}


vint8mf4_t test___riscv_vadc_vxm_i8mf4(vint8mf4_t op1,int8_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i8mf4(op1,op2,carryin,31);
}


vint8mf2_t test___riscv_vadc_vxm_i8mf2(vint8mf2_t op1,int8_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i8mf2(op1,op2,carryin,31);
}


vint8m1_t test___riscv_vadc_vxm_i8m1(vint8m1_t op1,int8_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i8m1(op1,op2,carryin,31);
}


vint8m2_t test___riscv_vadc_vxm_i8m2(vint8m2_t op1,int8_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i8m2(op1,op2,carryin,31);
}


vint8m4_t test___riscv_vadc_vxm_i8m4(vint8m4_t op1,int8_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i8m4(op1,op2,carryin,31);
}


vint8m8_t test___riscv_vadc_vxm_i8m8(vint8m8_t op1,int8_t op2,vbool1_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i8m8(op1,op2,carryin,31);
}


vint16mf4_t test___riscv_vadc_vxm_i16mf4(vint16mf4_t op1,int16_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i16mf4(op1,op2,carryin,31);
}


vint16mf2_t test___riscv_vadc_vxm_i16mf2(vint16mf2_t op1,int16_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i16mf2(op1,op2,carryin,31);
}


vint16m1_t test___riscv_vadc_vxm_i16m1(vint16m1_t op1,int16_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i16m1(op1,op2,carryin,31);
}


vint16m2_t test___riscv_vadc_vxm_i16m2(vint16m2_t op1,int16_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i16m2(op1,op2,carryin,31);
}


vint16m4_t test___riscv_vadc_vxm_i16m4(vint16m4_t op1,int16_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i16m4(op1,op2,carryin,31);
}


vint16m8_t test___riscv_vadc_vxm_i16m8(vint16m8_t op1,int16_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i16m8(op1,op2,carryin,31);
}


vint32mf2_t test___riscv_vadc_vxm_i32mf2(vint32mf2_t op1,int32_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i32mf2(op1,op2,carryin,31);
}


vint32m1_t test___riscv_vadc_vxm_i32m1(vint32m1_t op1,int32_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i32m1(op1,op2,carryin,31);
}


vint32m2_t test___riscv_vadc_vxm_i32m2(vint32m2_t op1,int32_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i32m2(op1,op2,carryin,31);
}


vint32m4_t test___riscv_vadc_vxm_i32m4(vint32m4_t op1,int32_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i32m4(op1,op2,carryin,31);
}


vint32m8_t test___riscv_vadc_vxm_i32m8(vint32m8_t op1,int32_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i32m8(op1,op2,carryin,31);
}


vint64m1_t test___riscv_vadc_vxm_i64m1(vint64m1_t op1,int64_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i64m1(op1,op2,carryin,31);
}


vint64m2_t test___riscv_vadc_vxm_i64m2(vint64m2_t op1,int64_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i64m2(op1,op2,carryin,31);
}


vint64m4_t test___riscv_vadc_vxm_i64m4(vint64m4_t op1,int64_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i64m4(op1,op2,carryin,31);
}


vint64m8_t test___riscv_vadc_vxm_i64m8(vint64m8_t op1,int64_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_i64m8(op1,op2,carryin,31);
}


vuint8mf8_t test___riscv_vadc_vxm_u8mf8(vuint8mf8_t op1,uint8_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u8mf8(op1,op2,carryin,31);
}


vuint8mf4_t test___riscv_vadc_vxm_u8mf4(vuint8mf4_t op1,uint8_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u8mf4(op1,op2,carryin,31);
}


vuint8mf2_t test___riscv_vadc_vxm_u8mf2(vuint8mf2_t op1,uint8_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u8mf2(op1,op2,carryin,31);
}


vuint8m1_t test___riscv_vadc_vxm_u8m1(vuint8m1_t op1,uint8_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u8m1(op1,op2,carryin,31);
}


vuint8m2_t test___riscv_vadc_vxm_u8m2(vuint8m2_t op1,uint8_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u8m2(op1,op2,carryin,31);
}


vuint8m4_t test___riscv_vadc_vxm_u8m4(vuint8m4_t op1,uint8_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u8m4(op1,op2,carryin,31);
}


vuint8m8_t test___riscv_vadc_vxm_u8m8(vuint8m8_t op1,uint8_t op2,vbool1_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u8m8(op1,op2,carryin,31);
}


vuint16mf4_t test___riscv_vadc_vxm_u16mf4(vuint16mf4_t op1,uint16_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u16mf4(op1,op2,carryin,31);
}


vuint16mf2_t test___riscv_vadc_vxm_u16mf2(vuint16mf2_t op1,uint16_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u16mf2(op1,op2,carryin,31);
}


vuint16m1_t test___riscv_vadc_vxm_u16m1(vuint16m1_t op1,uint16_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u16m1(op1,op2,carryin,31);
}


vuint16m2_t test___riscv_vadc_vxm_u16m2(vuint16m2_t op1,uint16_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u16m2(op1,op2,carryin,31);
}


vuint16m4_t test___riscv_vadc_vxm_u16m4(vuint16m4_t op1,uint16_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u16m4(op1,op2,carryin,31);
}


vuint16m8_t test___riscv_vadc_vxm_u16m8(vuint16m8_t op1,uint16_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u16m8(op1,op2,carryin,31);
}


vuint32mf2_t test___riscv_vadc_vxm_u32mf2(vuint32mf2_t op1,uint32_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u32mf2(op1,op2,carryin,31);
}


vuint32m1_t test___riscv_vadc_vxm_u32m1(vuint32m1_t op1,uint32_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u32m1(op1,op2,carryin,31);
}


vuint32m2_t test___riscv_vadc_vxm_u32m2(vuint32m2_t op1,uint32_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u32m2(op1,op2,carryin,31);
}


vuint32m4_t test___riscv_vadc_vxm_u32m4(vuint32m4_t op1,uint32_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u32m4(op1,op2,carryin,31);
}


vuint32m8_t test___riscv_vadc_vxm_u32m8(vuint32m8_t op1,uint32_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u32m8(op1,op2,carryin,31);
}


vuint64m1_t test___riscv_vadc_vxm_u64m1(vuint64m1_t op1,uint64_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u64m1(op1,op2,carryin,31);
}


vuint64m2_t test___riscv_vadc_vxm_u64m2(vuint64m2_t op1,uint64_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u64m2(op1,op2,carryin,31);
}


vuint64m4_t test___riscv_vadc_vxm_u64m4(vuint64m4_t op1,uint64_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u64m4(op1,op2,carryin,31);
}


vuint64m8_t test___riscv_vadc_vxm_u64m8(vuint64m8_t op1,uint64_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc_vxm_u64m8(op1,op2,carryin,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 8 } } */
