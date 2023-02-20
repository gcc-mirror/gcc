/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vncvt_x(vint16mf4_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint8mf4_t test___riscv_vncvt_x(vint16mf2_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint8mf2_t test___riscv_vncvt_x(vint16m1_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint8m1_t test___riscv_vncvt_x(vint16m2_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint8m2_t test___riscv_vncvt_x(vint16m4_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint8m4_t test___riscv_vncvt_x(vint16m8_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint8mf8_t test___riscv_vncvt_x(vuint16mf4_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint8mf4_t test___riscv_vncvt_x(vuint16mf2_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint8mf2_t test___riscv_vncvt_x(vuint16m1_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint8m1_t test___riscv_vncvt_x(vuint16m2_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint8m2_t test___riscv_vncvt_x(vuint16m4_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint8m4_t test___riscv_vncvt_x(vuint16m8_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint16mf4_t test___riscv_vncvt_x(vint32mf2_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint16mf2_t test___riscv_vncvt_x(vint32m1_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint16m1_t test___riscv_vncvt_x(vint32m2_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint16m2_t test___riscv_vncvt_x(vint32m4_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint16m4_t test___riscv_vncvt_x(vint32m8_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint16mf4_t test___riscv_vncvt_x(vuint32mf2_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint16mf2_t test___riscv_vncvt_x(vuint32m1_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint16m1_t test___riscv_vncvt_x(vuint32m2_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint16m2_t test___riscv_vncvt_x(vuint32m4_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint16m4_t test___riscv_vncvt_x(vuint32m8_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint32mf2_t test___riscv_vncvt_x(vint64m1_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint32m1_t test___riscv_vncvt_x(vint64m2_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint32m2_t test___riscv_vncvt_x(vint64m4_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint32m4_t test___riscv_vncvt_x(vint64m8_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint32mf2_t test___riscv_vncvt_x(vuint64m1_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint32m1_t test___riscv_vncvt_x(vuint64m2_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint32m2_t test___riscv_vncvt_x(vuint64m4_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vuint32m4_t test___riscv_vncvt_x(vuint64m8_t src,size_t vl)
{
    return __riscv_vncvt_x(src,31);
}


vint8mf8_t test___riscv_vncvt_x(vbool64_t mask,vint16mf4_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint8mf4_t test___riscv_vncvt_x(vbool32_t mask,vint16mf2_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint8mf2_t test___riscv_vncvt_x(vbool16_t mask,vint16m1_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint8m1_t test___riscv_vncvt_x(vbool8_t mask,vint16m2_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint8m2_t test___riscv_vncvt_x(vbool4_t mask,vint16m4_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint8m4_t test___riscv_vncvt_x(vbool2_t mask,vint16m8_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint8mf8_t test___riscv_vncvt_x(vbool64_t mask,vuint16mf4_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint8mf4_t test___riscv_vncvt_x(vbool32_t mask,vuint16mf2_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint8mf2_t test___riscv_vncvt_x(vbool16_t mask,vuint16m1_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint8m1_t test___riscv_vncvt_x(vbool8_t mask,vuint16m2_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint8m2_t test___riscv_vncvt_x(vbool4_t mask,vuint16m4_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint8m4_t test___riscv_vncvt_x(vbool2_t mask,vuint16m8_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint16mf4_t test___riscv_vncvt_x(vbool64_t mask,vint32mf2_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint16mf2_t test___riscv_vncvt_x(vbool32_t mask,vint32m1_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint16m1_t test___riscv_vncvt_x(vbool16_t mask,vint32m2_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint16m2_t test___riscv_vncvt_x(vbool8_t mask,vint32m4_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint16m4_t test___riscv_vncvt_x(vbool4_t mask,vint32m8_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint16mf4_t test___riscv_vncvt_x(vbool64_t mask,vuint32mf2_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint16mf2_t test___riscv_vncvt_x(vbool32_t mask,vuint32m1_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint16m1_t test___riscv_vncvt_x(vbool16_t mask,vuint32m2_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint16m2_t test___riscv_vncvt_x(vbool8_t mask,vuint32m4_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint16m4_t test___riscv_vncvt_x(vbool4_t mask,vuint32m8_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint32mf2_t test___riscv_vncvt_x(vbool64_t mask,vint64m1_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint32m1_t test___riscv_vncvt_x(vbool32_t mask,vint64m2_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint32m2_t test___riscv_vncvt_x(vbool16_t mask,vint64m4_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vint32m4_t test___riscv_vncvt_x(vbool8_t mask,vint64m8_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint32mf2_t test___riscv_vncvt_x(vbool64_t mask,vuint64m1_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint32m1_t test___riscv_vncvt_x(vbool32_t mask,vuint64m2_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint32m2_t test___riscv_vncvt_x(vbool16_t mask,vuint64m4_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}


vuint32m4_t test___riscv_vncvt_x(vbool8_t mask,vuint64m8_t src,size_t vl)
{
    return __riscv_vncvt_x(mask,src,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
