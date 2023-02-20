/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmaccsu(vint16mf4_t vd,vint8mf8_t vs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint16mf2_t test___riscv_vwmaccsu(vint16mf2_t vd,vint8mf4_t vs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint16m1_t test___riscv_vwmaccsu(vint16m1_t vd,vint8mf2_t vs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint16m2_t test___riscv_vwmaccsu(vint16m2_t vd,vint8m1_t vs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint16m4_t test___riscv_vwmaccsu(vint16m4_t vd,vint8m2_t vs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint16m8_t test___riscv_vwmaccsu(vint16m8_t vd,vint8m4_t vs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint32mf2_t test___riscv_vwmaccsu(vint32mf2_t vd,vint16mf4_t vs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint32m1_t test___riscv_vwmaccsu(vint32m1_t vd,vint16mf2_t vs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint32m2_t test___riscv_vwmaccsu(vint32m2_t vd,vint16m1_t vs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint32m4_t test___riscv_vwmaccsu(vint32m4_t vd,vint16m2_t vs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint32m8_t test___riscv_vwmaccsu(vint32m8_t vd,vint16m4_t vs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint64m1_t test___riscv_vwmaccsu(vint64m1_t vd,vint32mf2_t vs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint64m2_t test___riscv_vwmaccsu(vint64m2_t vd,vint32m1_t vs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint64m4_t test___riscv_vwmaccsu(vint64m4_t vd,vint32m2_t vs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint64m8_t test___riscv_vwmaccsu(vint64m8_t vd,vint32m4_t vs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(vd,vs1,vs2,31);
}


vint16mf4_t test___riscv_vwmaccsu(vbool64_t mask,vint16mf4_t vd,vint8mf8_t vs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint16mf2_t test___riscv_vwmaccsu(vbool32_t mask,vint16mf2_t vd,vint8mf4_t vs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint16m1_t test___riscv_vwmaccsu(vbool16_t mask,vint16m1_t vd,vint8mf2_t vs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint16m2_t test___riscv_vwmaccsu(vbool8_t mask,vint16m2_t vd,vint8m1_t vs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint16m4_t test___riscv_vwmaccsu(vbool4_t mask,vint16m4_t vd,vint8m2_t vs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint16m8_t test___riscv_vwmaccsu(vbool2_t mask,vint16m8_t vd,vint8m4_t vs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint32mf2_t test___riscv_vwmaccsu(vbool64_t mask,vint32mf2_t vd,vint16mf4_t vs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint32m1_t test___riscv_vwmaccsu(vbool32_t mask,vint32m1_t vd,vint16mf2_t vs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint32m2_t test___riscv_vwmaccsu(vbool16_t mask,vint32m2_t vd,vint16m1_t vs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint32m4_t test___riscv_vwmaccsu(vbool8_t mask,vint32m4_t vd,vint16m2_t vs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint32m8_t test___riscv_vwmaccsu(vbool4_t mask,vint32m8_t vd,vint16m4_t vs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint64m1_t test___riscv_vwmaccsu(vbool64_t mask,vint64m1_t vd,vint32mf2_t vs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint64m2_t test___riscv_vwmaccsu(vbool32_t mask,vint64m2_t vd,vint32m1_t vs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint64m4_t test___riscv_vwmaccsu(vbool16_t mask,vint64m4_t vd,vint32m2_t vs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}


vint64m8_t test___riscv_vwmaccsu(vbool8_t mask,vint64m8_t vd,vint32m4_t vs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu(mask,vd,vs1,vs2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwmaccsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
