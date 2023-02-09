/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vncvt_x_x_w_i8mf8(vint16mf4_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i8mf8(src,vl);
}


vint8mf4_t test___riscv_vncvt_x_x_w_i8mf4(vint16mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i8mf4(src,vl);
}


vint8mf2_t test___riscv_vncvt_x_x_w_i8mf2(vint16m1_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i8mf2(src,vl);
}


vint8m1_t test___riscv_vncvt_x_x_w_i8m1(vint16m2_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i8m1(src,vl);
}


vint8m2_t test___riscv_vncvt_x_x_w_i8m2(vint16m4_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i8m2(src,vl);
}


vint8m4_t test___riscv_vncvt_x_x_w_i8m4(vint16m8_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i8m4(src,vl);
}


vuint8mf8_t test___riscv_vncvt_x_x_w_u8mf8(vuint16mf4_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u8mf8(src,vl);
}


vuint8mf4_t test___riscv_vncvt_x_x_w_u8mf4(vuint16mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u8mf4(src,vl);
}


vuint8mf2_t test___riscv_vncvt_x_x_w_u8mf2(vuint16m1_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u8mf2(src,vl);
}


vuint8m1_t test___riscv_vncvt_x_x_w_u8m1(vuint16m2_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u8m1(src,vl);
}


vuint8m2_t test___riscv_vncvt_x_x_w_u8m2(vuint16m4_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u8m2(src,vl);
}


vuint8m4_t test___riscv_vncvt_x_x_w_u8m4(vuint16m8_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u8m4(src,vl);
}


vint16mf4_t test___riscv_vncvt_x_x_w_i16mf4(vint32mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i16mf4(src,vl);
}


vint16mf2_t test___riscv_vncvt_x_x_w_i16mf2(vint32m1_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i16mf2(src,vl);
}


vint16m1_t test___riscv_vncvt_x_x_w_i16m1(vint32m2_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i16m1(src,vl);
}


vint16m2_t test___riscv_vncvt_x_x_w_i16m2(vint32m4_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i16m2(src,vl);
}


vint16m4_t test___riscv_vncvt_x_x_w_i16m4(vint32m8_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i16m4(src,vl);
}


vuint16mf4_t test___riscv_vncvt_x_x_w_u16mf4(vuint32mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u16mf4(src,vl);
}


vuint16mf2_t test___riscv_vncvt_x_x_w_u16mf2(vuint32m1_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u16mf2(src,vl);
}


vuint16m1_t test___riscv_vncvt_x_x_w_u16m1(vuint32m2_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u16m1(src,vl);
}


vuint16m2_t test___riscv_vncvt_x_x_w_u16m2(vuint32m4_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u16m2(src,vl);
}


vuint16m4_t test___riscv_vncvt_x_x_w_u16m4(vuint32m8_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u16m4(src,vl);
}


vint32mf2_t test___riscv_vncvt_x_x_w_i32mf2(vint64m1_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i32mf2(src,vl);
}


vint32m1_t test___riscv_vncvt_x_x_w_i32m1(vint64m2_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i32m1(src,vl);
}


vint32m2_t test___riscv_vncvt_x_x_w_i32m2(vint64m4_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i32m2(src,vl);
}


vint32m4_t test___riscv_vncvt_x_x_w_i32m4(vint64m8_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_i32m4(src,vl);
}


vuint32mf2_t test___riscv_vncvt_x_x_w_u32mf2(vuint64m1_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u32mf2(src,vl);
}


vuint32m1_t test___riscv_vncvt_x_x_w_u32m1(vuint64m2_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u32m1(src,vl);
}


vuint32m2_t test___riscv_vncvt_x_x_w_u32m2(vuint64m4_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u32m2(src,vl);
}


vuint32m4_t test___riscv_vncvt_x_x_w_u32m4(vuint64m8_t src,size_t vl)
{
    return __riscv_vncvt_x_x_w_u32m4(src,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
