/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vrsub_vx_i8mf8_tumu(vbool64_t mask,vint8mf8_t merge,vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i8mf8_tumu(mask,merge,op1,op2,31);
}


vint8mf4_t test___riscv_vrsub_vx_i8mf4_tumu(vbool32_t mask,vint8mf4_t merge,vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i8mf4_tumu(mask,merge,op1,op2,31);
}


vint8mf2_t test___riscv_vrsub_vx_i8mf2_tumu(vbool16_t mask,vint8mf2_t merge,vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i8mf2_tumu(mask,merge,op1,op2,31);
}


vint8m1_t test___riscv_vrsub_vx_i8m1_tumu(vbool8_t mask,vint8m1_t merge,vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i8m1_tumu(mask,merge,op1,op2,31);
}


vint8m2_t test___riscv_vrsub_vx_i8m2_tumu(vbool4_t mask,vint8m2_t merge,vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i8m2_tumu(mask,merge,op1,op2,31);
}


vint8m4_t test___riscv_vrsub_vx_i8m4_tumu(vbool2_t mask,vint8m4_t merge,vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i8m4_tumu(mask,merge,op1,op2,31);
}


vint8m8_t test___riscv_vrsub_vx_i8m8_tumu(vbool1_t mask,vint8m8_t merge,vint8m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i8m8_tumu(mask,merge,op1,op2,31);
}


vint16mf4_t test___riscv_vrsub_vx_i16mf4_tumu(vbool64_t mask,vint16mf4_t merge,vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i16mf4_tumu(mask,merge,op1,op2,31);
}


vint16mf2_t test___riscv_vrsub_vx_i16mf2_tumu(vbool32_t mask,vint16mf2_t merge,vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i16mf2_tumu(mask,merge,op1,op2,31);
}


vint16m1_t test___riscv_vrsub_vx_i16m1_tumu(vbool16_t mask,vint16m1_t merge,vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i16m1_tumu(mask,merge,op1,op2,31);
}


vint16m2_t test___riscv_vrsub_vx_i16m2_tumu(vbool8_t mask,vint16m2_t merge,vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i16m2_tumu(mask,merge,op1,op2,31);
}


vint16m4_t test___riscv_vrsub_vx_i16m4_tumu(vbool4_t mask,vint16m4_t merge,vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i16m4_tumu(mask,merge,op1,op2,31);
}


vint16m8_t test___riscv_vrsub_vx_i16m8_tumu(vbool2_t mask,vint16m8_t merge,vint16m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i16m8_tumu(mask,merge,op1,op2,31);
}


vint32mf2_t test___riscv_vrsub_vx_i32mf2_tumu(vbool64_t mask,vint32mf2_t merge,vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i32mf2_tumu(mask,merge,op1,op2,31);
}


vint32m1_t test___riscv_vrsub_vx_i32m1_tumu(vbool32_t mask,vint32m1_t merge,vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i32m1_tumu(mask,merge,op1,op2,31);
}


vint32m2_t test___riscv_vrsub_vx_i32m2_tumu(vbool16_t mask,vint32m2_t merge,vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i32m2_tumu(mask,merge,op1,op2,31);
}


vint32m4_t test___riscv_vrsub_vx_i32m4_tumu(vbool8_t mask,vint32m4_t merge,vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i32m4_tumu(mask,merge,op1,op2,31);
}


vint32m8_t test___riscv_vrsub_vx_i32m8_tumu(vbool4_t mask,vint32m8_t merge,vint32m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i32m8_tumu(mask,merge,op1,op2,31);
}


vint64m1_t test___riscv_vrsub_vx_i64m1_tumu(vbool64_t mask,vint64m1_t merge,vint64m1_t op1,int64_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i64m1_tumu(mask,merge,op1,op2,31);
}


vint64m2_t test___riscv_vrsub_vx_i64m2_tumu(vbool32_t mask,vint64m2_t merge,vint64m2_t op1,int64_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i64m2_tumu(mask,merge,op1,op2,31);
}


vint64m4_t test___riscv_vrsub_vx_i64m4_tumu(vbool16_t mask,vint64m4_t merge,vint64m4_t op1,int64_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i64m4_tumu(mask,merge,op1,op2,31);
}


vint64m8_t test___riscv_vrsub_vx_i64m8_tumu(vbool8_t mask,vint64m8_t merge,vint64m8_t op1,int64_t op2,size_t vl)
{
    return __riscv_vrsub_vx_i64m8_tumu(mask,merge,op1,op2,31);
}


vuint8mf8_t test___riscv_vrsub_vx_u8mf8_tumu(vbool64_t mask,vuint8mf8_t merge,vuint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u8mf8_tumu(mask,merge,op1,op2,31);
}


vuint8mf4_t test___riscv_vrsub_vx_u8mf4_tumu(vbool32_t mask,vuint8mf4_t merge,vuint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u8mf4_tumu(mask,merge,op1,op2,31);
}


vuint8mf2_t test___riscv_vrsub_vx_u8mf2_tumu(vbool16_t mask,vuint8mf2_t merge,vuint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u8mf2_tumu(mask,merge,op1,op2,31);
}


vuint8m1_t test___riscv_vrsub_vx_u8m1_tumu(vbool8_t mask,vuint8m1_t merge,vuint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u8m1_tumu(mask,merge,op1,op2,31);
}


vuint8m2_t test___riscv_vrsub_vx_u8m2_tumu(vbool4_t mask,vuint8m2_t merge,vuint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u8m2_tumu(mask,merge,op1,op2,31);
}


vuint8m4_t test___riscv_vrsub_vx_u8m4_tumu(vbool2_t mask,vuint8m4_t merge,vuint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u8m4_tumu(mask,merge,op1,op2,31);
}


vuint8m8_t test___riscv_vrsub_vx_u8m8_tumu(vbool1_t mask,vuint8m8_t merge,vuint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u8m8_tumu(mask,merge,op1,op2,31);
}


vuint16mf4_t test___riscv_vrsub_vx_u16mf4_tumu(vbool64_t mask,vuint16mf4_t merge,vuint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u16mf4_tumu(mask,merge,op1,op2,31);
}


vuint16mf2_t test___riscv_vrsub_vx_u16mf2_tumu(vbool32_t mask,vuint16mf2_t merge,vuint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u16mf2_tumu(mask,merge,op1,op2,31);
}


vuint16m1_t test___riscv_vrsub_vx_u16m1_tumu(vbool16_t mask,vuint16m1_t merge,vuint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u16m1_tumu(mask,merge,op1,op2,31);
}


vuint16m2_t test___riscv_vrsub_vx_u16m2_tumu(vbool8_t mask,vuint16m2_t merge,vuint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u16m2_tumu(mask,merge,op1,op2,31);
}


vuint16m4_t test___riscv_vrsub_vx_u16m4_tumu(vbool4_t mask,vuint16m4_t merge,vuint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u16m4_tumu(mask,merge,op1,op2,31);
}


vuint16m8_t test___riscv_vrsub_vx_u16m8_tumu(vbool2_t mask,vuint16m8_t merge,vuint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u16m8_tumu(mask,merge,op1,op2,31);
}


vuint32mf2_t test___riscv_vrsub_vx_u32mf2_tumu(vbool64_t mask,vuint32mf2_t merge,vuint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u32mf2_tumu(mask,merge,op1,op2,31);
}


vuint32m1_t test___riscv_vrsub_vx_u32m1_tumu(vbool32_t mask,vuint32m1_t merge,vuint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u32m1_tumu(mask,merge,op1,op2,31);
}


vuint32m2_t test___riscv_vrsub_vx_u32m2_tumu(vbool16_t mask,vuint32m2_t merge,vuint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u32m2_tumu(mask,merge,op1,op2,31);
}


vuint32m4_t test___riscv_vrsub_vx_u32m4_tumu(vbool8_t mask,vuint32m4_t merge,vuint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u32m4_tumu(mask,merge,op1,op2,31);
}


vuint32m8_t test___riscv_vrsub_vx_u32m8_tumu(vbool4_t mask,vuint32m8_t merge,vuint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u32m8_tumu(mask,merge,op1,op2,31);
}


vuint64m1_t test___riscv_vrsub_vx_u64m1_tumu(vbool64_t mask,vuint64m1_t merge,vuint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u64m1_tumu(mask,merge,op1,op2,31);
}


vuint64m2_t test___riscv_vrsub_vx_u64m2_tumu(vbool32_t mask,vuint64m2_t merge,vuint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u64m2_tumu(mask,merge,op1,op2,31);
}


vuint64m4_t test___riscv_vrsub_vx_u64m4_tumu(vbool16_t mask,vuint64m4_t merge,vuint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u64m4_tumu(mask,merge,op1,op2,31);
}


vuint64m8_t test___riscv_vrsub_vx_u64m8_tumu(vbool8_t mask,vuint64m8_t merge,vuint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vrsub_vx_u64m8_tumu(mask,merge,op1,op2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*tu,\s*mu\s+vrsub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 2 } } */
