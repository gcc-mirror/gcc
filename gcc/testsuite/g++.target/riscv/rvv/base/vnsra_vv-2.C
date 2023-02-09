/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vnsra(vint16mf4_t op1,vuint8mf8_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint8mf4_t test___riscv_vnsra(vint16mf2_t op1,vuint8mf4_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint8mf2_t test___riscv_vnsra(vint16m1_t op1,vuint8mf2_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint8m1_t test___riscv_vnsra(vint16m2_t op1,vuint8m1_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint8m2_t test___riscv_vnsra(vint16m4_t op1,vuint8m2_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint8m4_t test___riscv_vnsra(vint16m8_t op1,vuint8m4_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint16mf4_t test___riscv_vnsra(vint32mf2_t op1,vuint16mf4_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint16mf2_t test___riscv_vnsra(vint32m1_t op1,vuint16mf2_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint16m1_t test___riscv_vnsra(vint32m2_t op1,vuint16m1_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint16m2_t test___riscv_vnsra(vint32m4_t op1,vuint16m2_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint16m4_t test___riscv_vnsra(vint32m8_t op1,vuint16m4_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint32mf2_t test___riscv_vnsra(vint64m1_t op1,vuint32mf2_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint32m1_t test___riscv_vnsra(vint64m2_t op1,vuint32m1_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint32m2_t test___riscv_vnsra(vint64m4_t op1,vuint32m2_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint32m4_t test___riscv_vnsra(vint64m8_t op1,vuint32m4_t shift,size_t vl)
{
    return __riscv_vnsra(op1,shift,31);
}


vint8mf8_t test___riscv_vnsra(vbool64_t mask,vint16mf4_t op1,vuint8mf8_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint8mf4_t test___riscv_vnsra(vbool32_t mask,vint16mf2_t op1,vuint8mf4_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint8mf2_t test___riscv_vnsra(vbool16_t mask,vint16m1_t op1,vuint8mf2_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint8m1_t test___riscv_vnsra(vbool8_t mask,vint16m2_t op1,vuint8m1_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint8m2_t test___riscv_vnsra(vbool4_t mask,vint16m4_t op1,vuint8m2_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint8m4_t test___riscv_vnsra(vbool2_t mask,vint16m8_t op1,vuint8m4_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint16mf4_t test___riscv_vnsra(vbool64_t mask,vint32mf2_t op1,vuint16mf4_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint16mf2_t test___riscv_vnsra(vbool32_t mask,vint32m1_t op1,vuint16mf2_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint16m1_t test___riscv_vnsra(vbool16_t mask,vint32m2_t op1,vuint16m1_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint16m2_t test___riscv_vnsra(vbool8_t mask,vint32m4_t op1,vuint16m2_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint16m4_t test___riscv_vnsra(vbool4_t mask,vint32m8_t op1,vuint16m4_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint32mf2_t test___riscv_vnsra(vbool64_t mask,vint64m1_t op1,vuint32mf2_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint32m1_t test___riscv_vnsra(vbool32_t mask,vint64m2_t op1,vuint32m1_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint32m2_t test___riscv_vnsra(vbool16_t mask,vint64m4_t op1,vuint32m2_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}


vint32m4_t test___riscv_vnsra(vbool8_t mask,vint64m8_t op1,vuint32m4_t shift,size_t vl)
{
    return __riscv_vnsra(mask,op1,shift,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
