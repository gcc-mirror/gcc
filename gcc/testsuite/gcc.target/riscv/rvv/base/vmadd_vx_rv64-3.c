/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vmadd_vx_i8mf8(vint8mf8_t vd,int8_t rs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i8mf8(vd,rs1,vs2,32);
}


vint8mf4_t test___riscv_vmadd_vx_i8mf4(vint8mf4_t vd,int8_t rs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i8mf4(vd,rs1,vs2,32);
}


vint8mf2_t test___riscv_vmadd_vx_i8mf2(vint8mf2_t vd,int8_t rs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i8mf2(vd,rs1,vs2,32);
}


vint8m1_t test___riscv_vmadd_vx_i8m1(vint8m1_t vd,int8_t rs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i8m1(vd,rs1,vs2,32);
}


vint8m2_t test___riscv_vmadd_vx_i8m2(vint8m2_t vd,int8_t rs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i8m2(vd,rs1,vs2,32);
}


vint8m4_t test___riscv_vmadd_vx_i8m4(vint8m4_t vd,int8_t rs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i8m4(vd,rs1,vs2,32);
}


vint8m8_t test___riscv_vmadd_vx_i8m8(vint8m8_t vd,int8_t rs1,vint8m8_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i8m8(vd,rs1,vs2,32);
}


vint16mf4_t test___riscv_vmadd_vx_i16mf4(vint16mf4_t vd,int16_t rs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i16mf4(vd,rs1,vs2,32);
}


vint16mf2_t test___riscv_vmadd_vx_i16mf2(vint16mf2_t vd,int16_t rs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i16mf2(vd,rs1,vs2,32);
}


vint16m1_t test___riscv_vmadd_vx_i16m1(vint16m1_t vd,int16_t rs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i16m1(vd,rs1,vs2,32);
}


vint16m2_t test___riscv_vmadd_vx_i16m2(vint16m2_t vd,int16_t rs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i16m2(vd,rs1,vs2,32);
}


vint16m4_t test___riscv_vmadd_vx_i16m4(vint16m4_t vd,int16_t rs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i16m4(vd,rs1,vs2,32);
}


vint16m8_t test___riscv_vmadd_vx_i16m8(vint16m8_t vd,int16_t rs1,vint16m8_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i16m8(vd,rs1,vs2,32);
}


vint32mf2_t test___riscv_vmadd_vx_i32mf2(vint32mf2_t vd,int32_t rs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i32mf2(vd,rs1,vs2,32);
}


vint32m1_t test___riscv_vmadd_vx_i32m1(vint32m1_t vd,int32_t rs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i32m1(vd,rs1,vs2,32);
}


vint32m2_t test___riscv_vmadd_vx_i32m2(vint32m2_t vd,int32_t rs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i32m2(vd,rs1,vs2,32);
}


vint32m4_t test___riscv_vmadd_vx_i32m4(vint32m4_t vd,int32_t rs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i32m4(vd,rs1,vs2,32);
}


vint32m8_t test___riscv_vmadd_vx_i32m8(vint32m8_t vd,int32_t rs1,vint32m8_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i32m8(vd,rs1,vs2,32);
}


vint64m1_t test___riscv_vmadd_vx_i64m1(vint64m1_t vd,int64_t rs1,vint64m1_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i64m1(vd,rs1,vs2,32);
}


vint64m2_t test___riscv_vmadd_vx_i64m2(vint64m2_t vd,int64_t rs1,vint64m2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i64m2(vd,rs1,vs2,32);
}


vint64m4_t test___riscv_vmadd_vx_i64m4(vint64m4_t vd,int64_t rs1,vint64m4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i64m4(vd,rs1,vs2,32);
}


vint64m8_t test___riscv_vmadd_vx_i64m8(vint64m8_t vd,int64_t rs1,vint64m8_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_i64m8(vd,rs1,vs2,32);
}


vuint8mf8_t test___riscv_vmadd_vx_u8mf8(vuint8mf8_t vd,uint8_t rs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u8mf8(vd,rs1,vs2,32);
}


vuint8mf4_t test___riscv_vmadd_vx_u8mf4(vuint8mf4_t vd,uint8_t rs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u8mf4(vd,rs1,vs2,32);
}


vuint8mf2_t test___riscv_vmadd_vx_u8mf2(vuint8mf2_t vd,uint8_t rs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u8mf2(vd,rs1,vs2,32);
}


vuint8m1_t test___riscv_vmadd_vx_u8m1(vuint8m1_t vd,uint8_t rs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u8m1(vd,rs1,vs2,32);
}


vuint8m2_t test___riscv_vmadd_vx_u8m2(vuint8m2_t vd,uint8_t rs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u8m2(vd,rs1,vs2,32);
}


vuint8m4_t test___riscv_vmadd_vx_u8m4(vuint8m4_t vd,uint8_t rs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u8m4(vd,rs1,vs2,32);
}


vuint8m8_t test___riscv_vmadd_vx_u8m8(vuint8m8_t vd,uint8_t rs1,vuint8m8_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u8m8(vd,rs1,vs2,32);
}


vuint16mf4_t test___riscv_vmadd_vx_u16mf4(vuint16mf4_t vd,uint16_t rs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u16mf4(vd,rs1,vs2,32);
}


vuint16mf2_t test___riscv_vmadd_vx_u16mf2(vuint16mf2_t vd,uint16_t rs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u16mf2(vd,rs1,vs2,32);
}


vuint16m1_t test___riscv_vmadd_vx_u16m1(vuint16m1_t vd,uint16_t rs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u16m1(vd,rs1,vs2,32);
}


vuint16m2_t test___riscv_vmadd_vx_u16m2(vuint16m2_t vd,uint16_t rs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u16m2(vd,rs1,vs2,32);
}


vuint16m4_t test___riscv_vmadd_vx_u16m4(vuint16m4_t vd,uint16_t rs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u16m4(vd,rs1,vs2,32);
}


vuint16m8_t test___riscv_vmadd_vx_u16m8(vuint16m8_t vd,uint16_t rs1,vuint16m8_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u16m8(vd,rs1,vs2,32);
}


vuint32mf2_t test___riscv_vmadd_vx_u32mf2(vuint32mf2_t vd,uint32_t rs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u32mf2(vd,rs1,vs2,32);
}


vuint32m1_t test___riscv_vmadd_vx_u32m1(vuint32m1_t vd,uint32_t rs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u32m1(vd,rs1,vs2,32);
}


vuint32m2_t test___riscv_vmadd_vx_u32m2(vuint32m2_t vd,uint32_t rs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u32m2(vd,rs1,vs2,32);
}


vuint32m4_t test___riscv_vmadd_vx_u32m4(vuint32m4_t vd,uint32_t rs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u32m4(vd,rs1,vs2,32);
}


vuint32m8_t test___riscv_vmadd_vx_u32m8(vuint32m8_t vd,uint32_t rs1,vuint32m8_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u32m8(vd,rs1,vs2,32);
}


vuint64m1_t test___riscv_vmadd_vx_u64m1(vuint64m1_t vd,uint64_t rs1,vuint64m1_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u64m1(vd,rs1,vs2,32);
}


vuint64m2_t test___riscv_vmadd_vx_u64m2(vuint64m2_t vd,uint64_t rs1,vuint64m2_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u64m2(vd,rs1,vs2,32);
}


vuint64m4_t test___riscv_vmadd_vx_u64m4(vuint64m4_t vd,uint64_t rs1,vuint64m4_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u64m4(vd,rs1,vs2,32);
}


vuint64m8_t test___riscv_vmadd_vx_u64m8(vuint64m8_t vd,uint64_t rs1,vuint64m8_t vs2,size_t vl)
{
    return __riscv_vmadd_vx_u64m8(vd,rs1,vs2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
