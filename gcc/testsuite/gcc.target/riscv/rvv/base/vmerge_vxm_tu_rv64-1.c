/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vmerge_vxm_i8mf8_tu(vint8mf8_t merge,vint8mf8_t op1,int8_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i8mf8_tu(merge,op1,op2,selector,vl);
}


vint8mf4_t test___riscv_vmerge_vxm_i8mf4_tu(vint8mf4_t merge,vint8mf4_t op1,int8_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i8mf4_tu(merge,op1,op2,selector,vl);
}


vint8mf2_t test___riscv_vmerge_vxm_i8mf2_tu(vint8mf2_t merge,vint8mf2_t op1,int8_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i8mf2_tu(merge,op1,op2,selector,vl);
}


vint8m1_t test___riscv_vmerge_vxm_i8m1_tu(vint8m1_t merge,vint8m1_t op1,int8_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i8m1_tu(merge,op1,op2,selector,vl);
}


vint8m2_t test___riscv_vmerge_vxm_i8m2_tu(vint8m2_t merge,vint8m2_t op1,int8_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i8m2_tu(merge,op1,op2,selector,vl);
}


vint8m4_t test___riscv_vmerge_vxm_i8m4_tu(vint8m4_t merge,vint8m4_t op1,int8_t op2,vbool2_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i8m4_tu(merge,op1,op2,selector,vl);
}


vint8m8_t test___riscv_vmerge_vxm_i8m8_tu(vint8m8_t merge,vint8m8_t op1,int8_t op2,vbool1_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i8m8_tu(merge,op1,op2,selector,vl);
}


vint16mf4_t test___riscv_vmerge_vxm_i16mf4_tu(vint16mf4_t merge,vint16mf4_t op1,int16_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i16mf4_tu(merge,op1,op2,selector,vl);
}


vint16mf2_t test___riscv_vmerge_vxm_i16mf2_tu(vint16mf2_t merge,vint16mf2_t op1,int16_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i16mf2_tu(merge,op1,op2,selector,vl);
}


vint16m1_t test___riscv_vmerge_vxm_i16m1_tu(vint16m1_t merge,vint16m1_t op1,int16_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i16m1_tu(merge,op1,op2,selector,vl);
}


vint16m2_t test___riscv_vmerge_vxm_i16m2_tu(vint16m2_t merge,vint16m2_t op1,int16_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i16m2_tu(merge,op1,op2,selector,vl);
}


vint16m4_t test___riscv_vmerge_vxm_i16m4_tu(vint16m4_t merge,vint16m4_t op1,int16_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i16m4_tu(merge,op1,op2,selector,vl);
}


vint16m8_t test___riscv_vmerge_vxm_i16m8_tu(vint16m8_t merge,vint16m8_t op1,int16_t op2,vbool2_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i16m8_tu(merge,op1,op2,selector,vl);
}


vint32mf2_t test___riscv_vmerge_vxm_i32mf2_tu(vint32mf2_t merge,vint32mf2_t op1,int32_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i32mf2_tu(merge,op1,op2,selector,vl);
}


vint32m1_t test___riscv_vmerge_vxm_i32m1_tu(vint32m1_t merge,vint32m1_t op1,int32_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i32m1_tu(merge,op1,op2,selector,vl);
}


vint32m2_t test___riscv_vmerge_vxm_i32m2_tu(vint32m2_t merge,vint32m2_t op1,int32_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i32m2_tu(merge,op1,op2,selector,vl);
}


vint32m4_t test___riscv_vmerge_vxm_i32m4_tu(vint32m4_t merge,vint32m4_t op1,int32_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i32m4_tu(merge,op1,op2,selector,vl);
}


vint32m8_t test___riscv_vmerge_vxm_i32m8_tu(vint32m8_t merge,vint32m8_t op1,int32_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i32m8_tu(merge,op1,op2,selector,vl);
}


vint64m1_t test___riscv_vmerge_vxm_i64m1_tu(vint64m1_t merge,vint64m1_t op1,int64_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i64m1_tu(merge,op1,op2,selector,vl);
}


vint64m2_t test___riscv_vmerge_vxm_i64m2_tu(vint64m2_t merge,vint64m2_t op1,int64_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i64m2_tu(merge,op1,op2,selector,vl);
}


vint64m4_t test___riscv_vmerge_vxm_i64m4_tu(vint64m4_t merge,vint64m4_t op1,int64_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i64m4_tu(merge,op1,op2,selector,vl);
}


vint64m8_t test___riscv_vmerge_vxm_i64m8_tu(vint64m8_t merge,vint64m8_t op1,int64_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_i64m8_tu(merge,op1,op2,selector,vl);
}


vuint8mf8_t test___riscv_vmerge_vxm_u8mf8_tu(vuint8mf8_t merge,vuint8mf8_t op1,uint8_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u8mf8_tu(merge,op1,op2,selector,vl);
}


vuint8mf4_t test___riscv_vmerge_vxm_u8mf4_tu(vuint8mf4_t merge,vuint8mf4_t op1,uint8_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u8mf4_tu(merge,op1,op2,selector,vl);
}


vuint8mf2_t test___riscv_vmerge_vxm_u8mf2_tu(vuint8mf2_t merge,vuint8mf2_t op1,uint8_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u8mf2_tu(merge,op1,op2,selector,vl);
}


vuint8m1_t test___riscv_vmerge_vxm_u8m1_tu(vuint8m1_t merge,vuint8m1_t op1,uint8_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u8m1_tu(merge,op1,op2,selector,vl);
}


vuint8m2_t test___riscv_vmerge_vxm_u8m2_tu(vuint8m2_t merge,vuint8m2_t op1,uint8_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u8m2_tu(merge,op1,op2,selector,vl);
}


vuint8m4_t test___riscv_vmerge_vxm_u8m4_tu(vuint8m4_t merge,vuint8m4_t op1,uint8_t op2,vbool2_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u8m4_tu(merge,op1,op2,selector,vl);
}


vuint8m8_t test___riscv_vmerge_vxm_u8m8_tu(vuint8m8_t merge,vuint8m8_t op1,uint8_t op2,vbool1_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u8m8_tu(merge,op1,op2,selector,vl);
}


vuint16mf4_t test___riscv_vmerge_vxm_u16mf4_tu(vuint16mf4_t merge,vuint16mf4_t op1,uint16_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u16mf4_tu(merge,op1,op2,selector,vl);
}


vuint16mf2_t test___riscv_vmerge_vxm_u16mf2_tu(vuint16mf2_t merge,vuint16mf2_t op1,uint16_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u16mf2_tu(merge,op1,op2,selector,vl);
}


vuint16m1_t test___riscv_vmerge_vxm_u16m1_tu(vuint16m1_t merge,vuint16m1_t op1,uint16_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u16m1_tu(merge,op1,op2,selector,vl);
}


vuint16m2_t test___riscv_vmerge_vxm_u16m2_tu(vuint16m2_t merge,vuint16m2_t op1,uint16_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u16m2_tu(merge,op1,op2,selector,vl);
}


vuint16m4_t test___riscv_vmerge_vxm_u16m4_tu(vuint16m4_t merge,vuint16m4_t op1,uint16_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u16m4_tu(merge,op1,op2,selector,vl);
}


vuint16m8_t test___riscv_vmerge_vxm_u16m8_tu(vuint16m8_t merge,vuint16m8_t op1,uint16_t op2,vbool2_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u16m8_tu(merge,op1,op2,selector,vl);
}


vuint32mf2_t test___riscv_vmerge_vxm_u32mf2_tu(vuint32mf2_t merge,vuint32mf2_t op1,uint32_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u32mf2_tu(merge,op1,op2,selector,vl);
}


vuint32m1_t test___riscv_vmerge_vxm_u32m1_tu(vuint32m1_t merge,vuint32m1_t op1,uint32_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u32m1_tu(merge,op1,op2,selector,vl);
}


vuint32m2_t test___riscv_vmerge_vxm_u32m2_tu(vuint32m2_t merge,vuint32m2_t op1,uint32_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u32m2_tu(merge,op1,op2,selector,vl);
}


vuint32m4_t test___riscv_vmerge_vxm_u32m4_tu(vuint32m4_t merge,vuint32m4_t op1,uint32_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u32m4_tu(merge,op1,op2,selector,vl);
}


vuint32m8_t test___riscv_vmerge_vxm_u32m8_tu(vuint32m8_t merge,vuint32m8_t op1,uint32_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u32m8_tu(merge,op1,op2,selector,vl);
}


vuint64m1_t test___riscv_vmerge_vxm_u64m1_tu(vuint64m1_t merge,vuint64m1_t op1,uint64_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u64m1_tu(merge,op1,op2,selector,vl);
}


vuint64m2_t test___riscv_vmerge_vxm_u64m2_tu(vuint64m2_t merge,vuint64m2_t op1,uint64_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u64m2_tu(merge,op1,op2,selector,vl);
}


vuint64m4_t test___riscv_vmerge_vxm_u64m4_tu(vuint64m4_t merge,vuint64m4_t op1,uint64_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u64m4_tu(merge,op1,op2,selector,vl);
}


vuint64m8_t test___riscv_vmerge_vxm_u64m8_tu(vuint64m8_t merge,vuint64m8_t op1,uint64_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vxm_u64m8_tu(merge,op1,op2,selector,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*tu,\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
