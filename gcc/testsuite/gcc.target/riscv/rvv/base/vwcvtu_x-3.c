/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint16mf4_t test___riscv_vwcvtu_x_x_v_u16mf4(vuint8mf8_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16mf4(src,32);
}


vuint16mf2_t test___riscv_vwcvtu_x_x_v_u16mf2(vuint8mf4_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16mf2(src,32);
}


vuint16m1_t test___riscv_vwcvtu_x_x_v_u16m1(vuint8mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16m1(src,32);
}


vuint16m2_t test___riscv_vwcvtu_x_x_v_u16m2(vuint8m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16m2(src,32);
}


vuint16m4_t test___riscv_vwcvtu_x_x_v_u16m4(vuint8m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16m4(src,32);
}


vuint16m8_t test___riscv_vwcvtu_x_x_v_u16m8(vuint8m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16m8(src,32);
}


vuint32mf2_t test___riscv_vwcvtu_x_x_v_u32mf2(vuint16mf4_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u32mf2(src,32);
}


vuint32m1_t test___riscv_vwcvtu_x_x_v_u32m1(vuint16mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u32m1(src,32);
}


vuint32m2_t test___riscv_vwcvtu_x_x_v_u32m2(vuint16m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u32m2(src,32);
}


vuint32m4_t test___riscv_vwcvtu_x_x_v_u32m4(vuint16m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u32m4(src,32);
}


vuint32m8_t test___riscv_vwcvtu_x_x_v_u32m8(vuint16m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u32m8(src,32);
}


vuint64m1_t test___riscv_vwcvtu_x_x_v_u64m1(vuint32mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u64m1(src,32);
}


vuint64m2_t test___riscv_vwcvtu_x_x_v_u64m2(vuint32m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u64m2(src,32);
}


vuint64m4_t test___riscv_vwcvtu_x_x_v_u64m4(vuint32m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u64m4(src,32);
}


vuint64m8_t test___riscv_vwcvtu_x_x_v_u64m8(vuint32m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u64m8(src,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
