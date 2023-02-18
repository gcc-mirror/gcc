/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vncvt_x_tu(vint8mf8_t merge,vint16mf4_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint8mf4_t test___riscv_vncvt_x_tu(vint8mf4_t merge,vint16mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint8mf2_t test___riscv_vncvt_x_tu(vint8mf2_t merge,vint16m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint8m1_t test___riscv_vncvt_x_tu(vint8m1_t merge,vint16m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint8m2_t test___riscv_vncvt_x_tu(vint8m2_t merge,vint16m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint8m4_t test___riscv_vncvt_x_tu(vint8m4_t merge,vint16m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint8mf8_t test___riscv_vncvt_x_tu(vuint8mf8_t merge,vuint16mf4_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint8mf4_t test___riscv_vncvt_x_tu(vuint8mf4_t merge,vuint16mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint8mf2_t test___riscv_vncvt_x_tu(vuint8mf2_t merge,vuint16m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint8m1_t test___riscv_vncvt_x_tu(vuint8m1_t merge,vuint16m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint8m2_t test___riscv_vncvt_x_tu(vuint8m2_t merge,vuint16m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint8m4_t test___riscv_vncvt_x_tu(vuint8m4_t merge,vuint16m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint16mf4_t test___riscv_vncvt_x_tu(vint16mf4_t merge,vint32mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint16mf2_t test___riscv_vncvt_x_tu(vint16mf2_t merge,vint32m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint16m1_t test___riscv_vncvt_x_tu(vint16m1_t merge,vint32m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint16m2_t test___riscv_vncvt_x_tu(vint16m2_t merge,vint32m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint16m4_t test___riscv_vncvt_x_tu(vint16m4_t merge,vint32m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint16mf4_t test___riscv_vncvt_x_tu(vuint16mf4_t merge,vuint32mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint16mf2_t test___riscv_vncvt_x_tu(vuint16mf2_t merge,vuint32m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint16m1_t test___riscv_vncvt_x_tu(vuint16m1_t merge,vuint32m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint16m2_t test___riscv_vncvt_x_tu(vuint16m2_t merge,vuint32m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint16m4_t test___riscv_vncvt_x_tu(vuint16m4_t merge,vuint32m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint32mf2_t test___riscv_vncvt_x_tu(vint32mf2_t merge,vint64m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint32m1_t test___riscv_vncvt_x_tu(vint32m1_t merge,vint64m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint32m2_t test___riscv_vncvt_x_tu(vint32m2_t merge,vint64m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vint32m4_t test___riscv_vncvt_x_tu(vint32m4_t merge,vint64m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint32mf2_t test___riscv_vncvt_x_tu(vuint32mf2_t merge,vuint64m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint32m1_t test___riscv_vncvt_x_tu(vuint32m1_t merge,vuint64m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint32m2_t test___riscv_vncvt_x_tu(vuint32m2_t merge,vuint64m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}


vuint32m4_t test___riscv_vncvt_x_tu(vuint32m4_t merge,vuint64m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tu(merge,src,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
