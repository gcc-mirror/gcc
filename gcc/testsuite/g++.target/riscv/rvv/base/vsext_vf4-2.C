/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint32mf2_t test___riscv_vsext_vf4(vint8mf8_t op1,size_t vl)
{
    return __riscv_vsext_vf4(op1,31);
}


vint32m1_t test___riscv_vsext_vf4(vint8mf4_t op1,size_t vl)
{
    return __riscv_vsext_vf4(op1,31);
}


vint32m2_t test___riscv_vsext_vf4(vint8mf2_t op1,size_t vl)
{
    return __riscv_vsext_vf4(op1,31);
}


vint32m4_t test___riscv_vsext_vf4(vint8m1_t op1,size_t vl)
{
    return __riscv_vsext_vf4(op1,31);
}


vint32m8_t test___riscv_vsext_vf4(vint8m2_t op1,size_t vl)
{
    return __riscv_vsext_vf4(op1,31);
}


vint64m1_t test___riscv_vsext_vf4(vint16mf4_t op1,size_t vl)
{
    return __riscv_vsext_vf4(op1,31);
}


vint64m2_t test___riscv_vsext_vf4(vint16mf2_t op1,size_t vl)
{
    return __riscv_vsext_vf4(op1,31);
}


vint64m4_t test___riscv_vsext_vf4(vint16m1_t op1,size_t vl)
{
    return __riscv_vsext_vf4(op1,31);
}


vint64m8_t test___riscv_vsext_vf4(vint16m2_t op1,size_t vl)
{
    return __riscv_vsext_vf4(op1,31);
}


vint32mf2_t test___riscv_vsext_vf4(vbool64_t mask,vint8mf8_t op1,size_t vl)
{
    return __riscv_vsext_vf4(mask,op1,31);
}


vint32m1_t test___riscv_vsext_vf4(vbool32_t mask,vint8mf4_t op1,size_t vl)
{
    return __riscv_vsext_vf4(mask,op1,31);
}


vint32m2_t test___riscv_vsext_vf4(vbool16_t mask,vint8mf2_t op1,size_t vl)
{
    return __riscv_vsext_vf4(mask,op1,31);
}


vint32m4_t test___riscv_vsext_vf4(vbool8_t mask,vint8m1_t op1,size_t vl)
{
    return __riscv_vsext_vf4(mask,op1,31);
}


vint32m8_t test___riscv_vsext_vf4(vbool4_t mask,vint8m2_t op1,size_t vl)
{
    return __riscv_vsext_vf4(mask,op1,31);
}


vint64m1_t test___riscv_vsext_vf4(vbool64_t mask,vint16mf4_t op1,size_t vl)
{
    return __riscv_vsext_vf4(mask,op1,31);
}


vint64m2_t test___riscv_vsext_vf4(vbool32_t mask,vint16mf2_t op1,size_t vl)
{
    return __riscv_vsext_vf4(mask,op1,31);
}


vint64m4_t test___riscv_vsext_vf4(vbool16_t mask,vint16m1_t op1,size_t vl)
{
    return __riscv_vsext_vf4(mask,op1,31);
}


vint64m8_t test___riscv_vsext_vf4(vbool8_t mask,vint16m2_t op1,size_t vl)
{
    return __riscv_vsext_vf4(mask,op1,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
