/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint16mf4_t test___riscv_vwcvtu_x_x_v_u16mf4_tu(vuint16mf4_t merge,vuint8mf8_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16mf4_tu(merge,src,32);
}


vuint16mf2_t test___riscv_vwcvtu_x_x_v_u16mf2_tu(vuint16mf2_t merge,vuint8mf4_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16mf2_tu(merge,src,32);
}


vuint16m1_t test___riscv_vwcvtu_x_x_v_u16m1_tu(vuint16m1_t merge,vuint8mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16m1_tu(merge,src,32);
}


vuint16m2_t test___riscv_vwcvtu_x_x_v_u16m2_tu(vuint16m2_t merge,vuint8m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16m2_tu(merge,src,32);
}


vuint16m4_t test___riscv_vwcvtu_x_x_v_u16m4_tu(vuint16m4_t merge,vuint8m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16m4_tu(merge,src,32);
}


vuint16m8_t test___riscv_vwcvtu_x_x_v_u16m8_tu(vuint16m8_t merge,vuint8m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u16m8_tu(merge,src,32);
}


vuint32mf2_t test___riscv_vwcvtu_x_x_v_u32mf2_tu(vuint32mf2_t merge,vuint16mf4_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u32mf2_tu(merge,src,32);
}


vuint32m1_t test___riscv_vwcvtu_x_x_v_u32m1_tu(vuint32m1_t merge,vuint16mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u32m1_tu(merge,src,32);
}


vuint32m2_t test___riscv_vwcvtu_x_x_v_u32m2_tu(vuint32m2_t merge,vuint16m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u32m2_tu(merge,src,32);
}


vuint32m4_t test___riscv_vwcvtu_x_x_v_u32m4_tu(vuint32m4_t merge,vuint16m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u32m4_tu(merge,src,32);
}


vuint32m8_t test___riscv_vwcvtu_x_x_v_u32m8_tu(vuint32m8_t merge,vuint16m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u32m8_tu(merge,src,32);
}


vuint64m1_t test___riscv_vwcvtu_x_x_v_u64m1_tu(vuint64m1_t merge,vuint32mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u64m1_tu(merge,src,32);
}


vuint64m2_t test___riscv_vwcvtu_x_x_v_u64m2_tu(vuint64m2_t merge,vuint32m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u64m2_tu(merge,src,32);
}


vuint64m4_t test___riscv_vwcvtu_x_x_v_u64m4_tu(vuint64m4_t merge,vuint32m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u64m4_tu(merge,src,32);
}


vuint64m8_t test___riscv_vwcvtu_x_x_v_u64m8_tu(vuint64m8_t merge,vuint32m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x_x_v_u64m8_tu(merge,src,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+} 1 } } */
