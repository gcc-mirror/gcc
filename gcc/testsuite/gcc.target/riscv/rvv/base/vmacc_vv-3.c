/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vmacc_vv_i8mf8(vint8mf8_t vd,vint8mf8_t vs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i8mf8(vd,vs1,vs2,32);
}


vint8mf4_t test___riscv_vmacc_vv_i8mf4(vint8mf4_t vd,vint8mf4_t vs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i8mf4(vd,vs1,vs2,32);
}


vint8mf2_t test___riscv_vmacc_vv_i8mf2(vint8mf2_t vd,vint8mf2_t vs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i8mf2(vd,vs1,vs2,32);
}


vint8m1_t test___riscv_vmacc_vv_i8m1(vint8m1_t vd,vint8m1_t vs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i8m1(vd,vs1,vs2,32);
}


vint8m2_t test___riscv_vmacc_vv_i8m2(vint8m2_t vd,vint8m2_t vs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i8m2(vd,vs1,vs2,32);
}


vint8m4_t test___riscv_vmacc_vv_i8m4(vint8m4_t vd,vint8m4_t vs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i8m4(vd,vs1,vs2,32);
}


vint8m8_t test___riscv_vmacc_vv_i8m8(vint8m8_t vd,vint8m8_t vs1,vint8m8_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i8m8(vd,vs1,vs2,32);
}


vint16mf4_t test___riscv_vmacc_vv_i16mf4(vint16mf4_t vd,vint16mf4_t vs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i16mf4(vd,vs1,vs2,32);
}


vint16mf2_t test___riscv_vmacc_vv_i16mf2(vint16mf2_t vd,vint16mf2_t vs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i16mf2(vd,vs1,vs2,32);
}


vint16m1_t test___riscv_vmacc_vv_i16m1(vint16m1_t vd,vint16m1_t vs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i16m1(vd,vs1,vs2,32);
}


vint16m2_t test___riscv_vmacc_vv_i16m2(vint16m2_t vd,vint16m2_t vs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i16m2(vd,vs1,vs2,32);
}


vint16m4_t test___riscv_vmacc_vv_i16m4(vint16m4_t vd,vint16m4_t vs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i16m4(vd,vs1,vs2,32);
}


vint16m8_t test___riscv_vmacc_vv_i16m8(vint16m8_t vd,vint16m8_t vs1,vint16m8_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i16m8(vd,vs1,vs2,32);
}


vint32mf2_t test___riscv_vmacc_vv_i32mf2(vint32mf2_t vd,vint32mf2_t vs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i32mf2(vd,vs1,vs2,32);
}


vint32m1_t test___riscv_vmacc_vv_i32m1(vint32m1_t vd,vint32m1_t vs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i32m1(vd,vs1,vs2,32);
}


vint32m2_t test___riscv_vmacc_vv_i32m2(vint32m2_t vd,vint32m2_t vs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i32m2(vd,vs1,vs2,32);
}


vint32m4_t test___riscv_vmacc_vv_i32m4(vint32m4_t vd,vint32m4_t vs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i32m4(vd,vs1,vs2,32);
}


vint32m8_t test___riscv_vmacc_vv_i32m8(vint32m8_t vd,vint32m8_t vs1,vint32m8_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i32m8(vd,vs1,vs2,32);
}


vint64m1_t test___riscv_vmacc_vv_i64m1(vint64m1_t vd,vint64m1_t vs1,vint64m1_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i64m1(vd,vs1,vs2,32);
}


vint64m2_t test___riscv_vmacc_vv_i64m2(vint64m2_t vd,vint64m2_t vs1,vint64m2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i64m2(vd,vs1,vs2,32);
}


vint64m4_t test___riscv_vmacc_vv_i64m4(vint64m4_t vd,vint64m4_t vs1,vint64m4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i64m4(vd,vs1,vs2,32);
}


vint64m8_t test___riscv_vmacc_vv_i64m8(vint64m8_t vd,vint64m8_t vs1,vint64m8_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_i64m8(vd,vs1,vs2,32);
}


vuint8mf8_t test___riscv_vmacc_vv_u8mf8(vuint8mf8_t vd,vuint8mf8_t vs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u8mf8(vd,vs1,vs2,32);
}


vuint8mf4_t test___riscv_vmacc_vv_u8mf4(vuint8mf4_t vd,vuint8mf4_t vs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u8mf4(vd,vs1,vs2,32);
}


vuint8mf2_t test___riscv_vmacc_vv_u8mf2(vuint8mf2_t vd,vuint8mf2_t vs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u8mf2(vd,vs1,vs2,32);
}


vuint8m1_t test___riscv_vmacc_vv_u8m1(vuint8m1_t vd,vuint8m1_t vs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u8m1(vd,vs1,vs2,32);
}


vuint8m2_t test___riscv_vmacc_vv_u8m2(vuint8m2_t vd,vuint8m2_t vs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u8m2(vd,vs1,vs2,32);
}


vuint8m4_t test___riscv_vmacc_vv_u8m4(vuint8m4_t vd,vuint8m4_t vs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u8m4(vd,vs1,vs2,32);
}


vuint8m8_t test___riscv_vmacc_vv_u8m8(vuint8m8_t vd,vuint8m8_t vs1,vuint8m8_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u8m8(vd,vs1,vs2,32);
}


vuint16mf4_t test___riscv_vmacc_vv_u16mf4(vuint16mf4_t vd,vuint16mf4_t vs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u16mf4(vd,vs1,vs2,32);
}


vuint16mf2_t test___riscv_vmacc_vv_u16mf2(vuint16mf2_t vd,vuint16mf2_t vs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u16mf2(vd,vs1,vs2,32);
}


vuint16m1_t test___riscv_vmacc_vv_u16m1(vuint16m1_t vd,vuint16m1_t vs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u16m1(vd,vs1,vs2,32);
}


vuint16m2_t test___riscv_vmacc_vv_u16m2(vuint16m2_t vd,vuint16m2_t vs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u16m2(vd,vs1,vs2,32);
}


vuint16m4_t test___riscv_vmacc_vv_u16m4(vuint16m4_t vd,vuint16m4_t vs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u16m4(vd,vs1,vs2,32);
}


vuint16m8_t test___riscv_vmacc_vv_u16m8(vuint16m8_t vd,vuint16m8_t vs1,vuint16m8_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u16m8(vd,vs1,vs2,32);
}


vuint32mf2_t test___riscv_vmacc_vv_u32mf2(vuint32mf2_t vd,vuint32mf2_t vs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u32mf2(vd,vs1,vs2,32);
}


vuint32m1_t test___riscv_vmacc_vv_u32m1(vuint32m1_t vd,vuint32m1_t vs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u32m1(vd,vs1,vs2,32);
}


vuint32m2_t test___riscv_vmacc_vv_u32m2(vuint32m2_t vd,vuint32m2_t vs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u32m2(vd,vs1,vs2,32);
}


vuint32m4_t test___riscv_vmacc_vv_u32m4(vuint32m4_t vd,vuint32m4_t vs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u32m4(vd,vs1,vs2,32);
}


vuint32m8_t test___riscv_vmacc_vv_u32m8(vuint32m8_t vd,vuint32m8_t vs1,vuint32m8_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u32m8(vd,vs1,vs2,32);
}


vuint64m1_t test___riscv_vmacc_vv_u64m1(vuint64m1_t vd,vuint64m1_t vs1,vuint64m1_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u64m1(vd,vs1,vs2,32);
}


vuint64m2_t test___riscv_vmacc_vv_u64m2(vuint64m2_t vd,vuint64m2_t vs1,vuint64m2_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u64m2(vd,vs1,vs2,32);
}


vuint64m4_t test___riscv_vmacc_vv_u64m4(vuint64m4_t vd,vuint64m4_t vs1,vuint64m4_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u64m4(vd,vs1,vs2,32);
}


vuint64m8_t test___riscv_vmacc_vv_u64m8(vuint64m8_t vd,vuint64m8_t vs1,vuint64m8_t vs2,size_t vl)
{
    return __riscv_vmacc_vv_u64m8(vd,vs1,vs2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
