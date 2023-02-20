/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vssra(vint8mf8_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint8mf4_t test___riscv_vssra(vint8mf4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint8mf2_t test___riscv_vssra(vint8mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint8m1_t test___riscv_vssra(vint8m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint8m2_t test___riscv_vssra(vint8m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint8m4_t test___riscv_vssra(vint8m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint8m8_t test___riscv_vssra(vint8m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint16mf4_t test___riscv_vssra(vint16mf4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint16mf2_t test___riscv_vssra(vint16mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint16m1_t test___riscv_vssra(vint16m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint16m2_t test___riscv_vssra(vint16m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint16m4_t test___riscv_vssra(vint16m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint16m8_t test___riscv_vssra(vint16m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint32mf2_t test___riscv_vssra(vint32mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint32m1_t test___riscv_vssra(vint32m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint32m2_t test___riscv_vssra(vint32m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint32m4_t test___riscv_vssra(vint32m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint32m8_t test___riscv_vssra(vint32m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint64m1_t test___riscv_vssra(vint64m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint64m2_t test___riscv_vssra(vint64m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint64m4_t test___riscv_vssra(vint64m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint64m8_t test___riscv_vssra(vint64m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(op1,shift,31);
}


vint8mf8_t test___riscv_vssra(vbool64_t mask,vint8mf8_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint8mf4_t test___riscv_vssra(vbool32_t mask,vint8mf4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint8mf2_t test___riscv_vssra(vbool16_t mask,vint8mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint8m1_t test___riscv_vssra(vbool8_t mask,vint8m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint8m2_t test___riscv_vssra(vbool4_t mask,vint8m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint8m4_t test___riscv_vssra(vbool2_t mask,vint8m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint8m8_t test___riscv_vssra(vbool1_t mask,vint8m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint16mf4_t test___riscv_vssra(vbool64_t mask,vint16mf4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint16mf2_t test___riscv_vssra(vbool32_t mask,vint16mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint16m1_t test___riscv_vssra(vbool16_t mask,vint16m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint16m2_t test___riscv_vssra(vbool8_t mask,vint16m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint16m4_t test___riscv_vssra(vbool4_t mask,vint16m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint16m8_t test___riscv_vssra(vbool2_t mask,vint16m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint32mf2_t test___riscv_vssra(vbool64_t mask,vint32mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint32m1_t test___riscv_vssra(vbool32_t mask,vint32m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint32m2_t test___riscv_vssra(vbool16_t mask,vint32m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint32m4_t test___riscv_vssra(vbool8_t mask,vint32m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint32m8_t test___riscv_vssra(vbool4_t mask,vint32m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint64m1_t test___riscv_vssra(vbool64_t mask,vint64m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint64m2_t test___riscv_vssra(vbool32_t mask,vint64m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint64m4_t test___riscv_vssra(vbool16_t mask,vint64m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}


vint64m8_t test___riscv_vssra(vbool8_t mask,vint64m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vssra(mask,op1,shift,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vssra\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
