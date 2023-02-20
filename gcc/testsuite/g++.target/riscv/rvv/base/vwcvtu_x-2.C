/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint16mf4_t test___riscv_vwcvtu_x(vuint8mf8_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint16mf2_t test___riscv_vwcvtu_x(vuint8mf4_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint16m1_t test___riscv_vwcvtu_x(vuint8mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint16m2_t test___riscv_vwcvtu_x(vuint8m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint16m4_t test___riscv_vwcvtu_x(vuint8m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint16m8_t test___riscv_vwcvtu_x(vuint8m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint32mf2_t test___riscv_vwcvtu_x(vuint16mf4_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint32m1_t test___riscv_vwcvtu_x(vuint16mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint32m2_t test___riscv_vwcvtu_x(vuint16m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint32m4_t test___riscv_vwcvtu_x(vuint16m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint32m8_t test___riscv_vwcvtu_x(vuint16m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint64m1_t test___riscv_vwcvtu_x(vuint32mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint64m2_t test___riscv_vwcvtu_x(vuint32m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint64m4_t test___riscv_vwcvtu_x(vuint32m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint64m8_t test___riscv_vwcvtu_x(vuint32m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x(src,31);
}


vuint16mf4_t test___riscv_vwcvtu_x(vbool64_t mask,vuint8mf8_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint16mf2_t test___riscv_vwcvtu_x(vbool32_t mask,vuint8mf4_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint16m1_t test___riscv_vwcvtu_x(vbool16_t mask,vuint8mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint16m2_t test___riscv_vwcvtu_x(vbool8_t mask,vuint8m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint16m4_t test___riscv_vwcvtu_x(vbool4_t mask,vuint8m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint16m8_t test___riscv_vwcvtu_x(vbool2_t mask,vuint8m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint32mf2_t test___riscv_vwcvtu_x(vbool64_t mask,vuint16mf4_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint32m1_t test___riscv_vwcvtu_x(vbool32_t mask,vuint16mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint32m2_t test___riscv_vwcvtu_x(vbool16_t mask,vuint16m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint32m4_t test___riscv_vwcvtu_x(vbool8_t mask,vuint16m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint32m8_t test___riscv_vwcvtu_x(vbool4_t mask,vuint16m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint64m1_t test___riscv_vwcvtu_x(vbool64_t mask,vuint32mf2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint64m2_t test___riscv_vwcvtu_x(vbool32_t mask,vuint32m1_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint64m4_t test___riscv_vwcvtu_x(vbool16_t mask,vuint32m2_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}


vuint64m8_t test___riscv_vwcvtu_x(vbool8_t mask,vuint32m4_t src,size_t vl)
{
    return __riscv_vwcvtu_x(mask,src,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwcvtu\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
