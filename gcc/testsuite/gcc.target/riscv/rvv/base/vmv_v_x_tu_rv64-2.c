/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vmv_v_x_i8mf8_tu(vint8mf8_t merge,int8_t src,size_t vl)
{
    return __riscv_vmv_v_x_i8mf8_tu(merge,src,31);
}


vint8mf4_t test___riscv_vmv_v_x_i8mf4_tu(vint8mf4_t merge,int8_t src,size_t vl)
{
    return __riscv_vmv_v_x_i8mf4_tu(merge,src,31);
}


vint8mf2_t test___riscv_vmv_v_x_i8mf2_tu(vint8mf2_t merge,int8_t src,size_t vl)
{
    return __riscv_vmv_v_x_i8mf2_tu(merge,src,31);
}


vint8m1_t test___riscv_vmv_v_x_i8m1_tu(vint8m1_t merge,int8_t src,size_t vl)
{
    return __riscv_vmv_v_x_i8m1_tu(merge,src,31);
}


vint8m2_t test___riscv_vmv_v_x_i8m2_tu(vint8m2_t merge,int8_t src,size_t vl)
{
    return __riscv_vmv_v_x_i8m2_tu(merge,src,31);
}


vint8m4_t test___riscv_vmv_v_x_i8m4_tu(vint8m4_t merge,int8_t src,size_t vl)
{
    return __riscv_vmv_v_x_i8m4_tu(merge,src,31);
}


vint8m8_t test___riscv_vmv_v_x_i8m8_tu(vint8m8_t merge,int8_t src,size_t vl)
{
    return __riscv_vmv_v_x_i8m8_tu(merge,src,31);
}


vint16mf4_t test___riscv_vmv_v_x_i16mf4_tu(vint16mf4_t merge,int16_t src,size_t vl)
{
    return __riscv_vmv_v_x_i16mf4_tu(merge,src,31);
}


vint16mf2_t test___riscv_vmv_v_x_i16mf2_tu(vint16mf2_t merge,int16_t src,size_t vl)
{
    return __riscv_vmv_v_x_i16mf2_tu(merge,src,31);
}


vint16m1_t test___riscv_vmv_v_x_i16m1_tu(vint16m1_t merge,int16_t src,size_t vl)
{
    return __riscv_vmv_v_x_i16m1_tu(merge,src,31);
}


vint16m2_t test___riscv_vmv_v_x_i16m2_tu(vint16m2_t merge,int16_t src,size_t vl)
{
    return __riscv_vmv_v_x_i16m2_tu(merge,src,31);
}


vint16m4_t test___riscv_vmv_v_x_i16m4_tu(vint16m4_t merge,int16_t src,size_t vl)
{
    return __riscv_vmv_v_x_i16m4_tu(merge,src,31);
}


vint16m8_t test___riscv_vmv_v_x_i16m8_tu(vint16m8_t merge,int16_t src,size_t vl)
{
    return __riscv_vmv_v_x_i16m8_tu(merge,src,31);
}


vint32mf2_t test___riscv_vmv_v_x_i32mf2_tu(vint32mf2_t merge,int32_t src,size_t vl)
{
    return __riscv_vmv_v_x_i32mf2_tu(merge,src,31);
}


vint32m1_t test___riscv_vmv_v_x_i32m1_tu(vint32m1_t merge,int32_t src,size_t vl)
{
    return __riscv_vmv_v_x_i32m1_tu(merge,src,31);
}


vint32m2_t test___riscv_vmv_v_x_i32m2_tu(vint32m2_t merge,int32_t src,size_t vl)
{
    return __riscv_vmv_v_x_i32m2_tu(merge,src,31);
}


vint32m4_t test___riscv_vmv_v_x_i32m4_tu(vint32m4_t merge,int32_t src,size_t vl)
{
    return __riscv_vmv_v_x_i32m4_tu(merge,src,31);
}


vint32m8_t test___riscv_vmv_v_x_i32m8_tu(vint32m8_t merge,int32_t src,size_t vl)
{
    return __riscv_vmv_v_x_i32m8_tu(merge,src,31);
}


vint64m1_t test___riscv_vmv_v_x_i64m1_tu(vint64m1_t merge,int64_t src,size_t vl)
{
    return __riscv_vmv_v_x_i64m1_tu(merge,src,31);
}


vint64m2_t test___riscv_vmv_v_x_i64m2_tu(vint64m2_t merge,int64_t src,size_t vl)
{
    return __riscv_vmv_v_x_i64m2_tu(merge,src,31);
}


vint64m4_t test___riscv_vmv_v_x_i64m4_tu(vint64m4_t merge,int64_t src,size_t vl)
{
    return __riscv_vmv_v_x_i64m4_tu(merge,src,31);
}


vint64m8_t test___riscv_vmv_v_x_i64m8_tu(vint64m8_t merge,int64_t src,size_t vl)
{
    return __riscv_vmv_v_x_i64m8_tu(merge,src,31);
}


vuint8mf8_t test___riscv_vmv_v_x_u8mf8_tu(vuint8mf8_t merge,uint8_t src,size_t vl)
{
    return __riscv_vmv_v_x_u8mf8_tu(merge,src,31);
}


vuint8mf4_t test___riscv_vmv_v_x_u8mf4_tu(vuint8mf4_t merge,uint8_t src,size_t vl)
{
    return __riscv_vmv_v_x_u8mf4_tu(merge,src,31);
}


vuint8mf2_t test___riscv_vmv_v_x_u8mf2_tu(vuint8mf2_t merge,uint8_t src,size_t vl)
{
    return __riscv_vmv_v_x_u8mf2_tu(merge,src,31);
}


vuint8m1_t test___riscv_vmv_v_x_u8m1_tu(vuint8m1_t merge,uint8_t src,size_t vl)
{
    return __riscv_vmv_v_x_u8m1_tu(merge,src,31);
}


vuint8m2_t test___riscv_vmv_v_x_u8m2_tu(vuint8m2_t merge,uint8_t src,size_t vl)
{
    return __riscv_vmv_v_x_u8m2_tu(merge,src,31);
}


vuint8m4_t test___riscv_vmv_v_x_u8m4_tu(vuint8m4_t merge,uint8_t src,size_t vl)
{
    return __riscv_vmv_v_x_u8m4_tu(merge,src,31);
}


vuint8m8_t test___riscv_vmv_v_x_u8m8_tu(vuint8m8_t merge,uint8_t src,size_t vl)
{
    return __riscv_vmv_v_x_u8m8_tu(merge,src,31);
}


vuint16mf4_t test___riscv_vmv_v_x_u16mf4_tu(vuint16mf4_t merge,uint16_t src,size_t vl)
{
    return __riscv_vmv_v_x_u16mf4_tu(merge,src,31);
}


vuint16mf2_t test___riscv_vmv_v_x_u16mf2_tu(vuint16mf2_t merge,uint16_t src,size_t vl)
{
    return __riscv_vmv_v_x_u16mf2_tu(merge,src,31);
}


vuint16m1_t test___riscv_vmv_v_x_u16m1_tu(vuint16m1_t merge,uint16_t src,size_t vl)
{
    return __riscv_vmv_v_x_u16m1_tu(merge,src,31);
}


vuint16m2_t test___riscv_vmv_v_x_u16m2_tu(vuint16m2_t merge,uint16_t src,size_t vl)
{
    return __riscv_vmv_v_x_u16m2_tu(merge,src,31);
}


vuint16m4_t test___riscv_vmv_v_x_u16m4_tu(vuint16m4_t merge,uint16_t src,size_t vl)
{
    return __riscv_vmv_v_x_u16m4_tu(merge,src,31);
}


vuint16m8_t test___riscv_vmv_v_x_u16m8_tu(vuint16m8_t merge,uint16_t src,size_t vl)
{
    return __riscv_vmv_v_x_u16m8_tu(merge,src,31);
}


vuint32mf2_t test___riscv_vmv_v_x_u32mf2_tu(vuint32mf2_t merge,uint32_t src,size_t vl)
{
    return __riscv_vmv_v_x_u32mf2_tu(merge,src,31);
}


vuint32m1_t test___riscv_vmv_v_x_u32m1_tu(vuint32m1_t merge,uint32_t src,size_t vl)
{
    return __riscv_vmv_v_x_u32m1_tu(merge,src,31);
}


vuint32m2_t test___riscv_vmv_v_x_u32m2_tu(vuint32m2_t merge,uint32_t src,size_t vl)
{
    return __riscv_vmv_v_x_u32m2_tu(merge,src,31);
}


vuint32m4_t test___riscv_vmv_v_x_u32m4_tu(vuint32m4_t merge,uint32_t src,size_t vl)
{
    return __riscv_vmv_v_x_u32m4_tu(merge,src,31);
}


vuint32m8_t test___riscv_vmv_v_x_u32m8_tu(vuint32m8_t merge,uint32_t src,size_t vl)
{
    return __riscv_vmv_v_x_u32m8_tu(merge,src,31);
}


vuint64m1_t test___riscv_vmv_v_x_u64m1_tu(vuint64m1_t merge,uint64_t src,size_t vl)
{
    return __riscv_vmv_v_x_u64m1_tu(merge,src,31);
}


vuint64m2_t test___riscv_vmv_v_x_u64m2_tu(vuint64m2_t merge,uint64_t src,size_t vl)
{
    return __riscv_vmv_v_x_u64m2_tu(merge,src,31);
}


vuint64m4_t test___riscv_vmv_v_x_u64m4_tu(vuint64m4_t merge,uint64_t src,size_t vl)
{
    return __riscv_vmv_v_x_u64m4_tu(merge,src,31);
}


vuint64m8_t test___riscv_vmv_v_x_u64m8_tu(vuint64m8_t merge,uint64_t src,size_t vl)
{
    return __riscv_vmv_v_x_u64m8_tu(merge,src,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*tu,\s*m[au]\s+vmv\.v\.x\s+v[0-9]+,\s*[a-x0-9]+} 2 } } */
