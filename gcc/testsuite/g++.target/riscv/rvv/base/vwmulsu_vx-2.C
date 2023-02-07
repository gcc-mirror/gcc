/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmulsu(vint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint16mf2_t test___riscv_vwmulsu(vint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint16m1_t test___riscv_vwmulsu(vint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint16m2_t test___riscv_vwmulsu(vint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint16m4_t test___riscv_vwmulsu(vint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint16m8_t test___riscv_vwmulsu(vint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint32mf2_t test___riscv_vwmulsu(vint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint32m1_t test___riscv_vwmulsu(vint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint32m2_t test___riscv_vwmulsu(vint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint32m4_t test___riscv_vwmulsu(vint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint32m8_t test___riscv_vwmulsu(vint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint64m1_t test___riscv_vwmulsu(vint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint64m2_t test___riscv_vwmulsu(vint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint64m4_t test___riscv_vwmulsu(vint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint64m8_t test___riscv_vwmulsu(vint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwmulsu(op1,op2,31);
}


vint16mf4_t test___riscv_vwmulsu(vbool64_t mask,vint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint16mf2_t test___riscv_vwmulsu(vbool32_t mask,vint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint16m1_t test___riscv_vwmulsu(vbool16_t mask,vint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint16m2_t test___riscv_vwmulsu(vbool8_t mask,vint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint16m4_t test___riscv_vwmulsu(vbool4_t mask,vint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint16m8_t test___riscv_vwmulsu(vbool2_t mask,vint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint32mf2_t test___riscv_vwmulsu(vbool64_t mask,vint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint32m1_t test___riscv_vwmulsu(vbool32_t mask,vint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint32m2_t test___riscv_vwmulsu(vbool16_t mask,vint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint32m4_t test___riscv_vwmulsu(vbool8_t mask,vint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint32m8_t test___riscv_vwmulsu(vbool4_t mask,vint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint64m1_t test___riscv_vwmulsu(vbool64_t mask,vint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint64m2_t test___riscv_vwmulsu(vbool32_t mask,vint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint64m4_t test___riscv_vwmulsu(vbool16_t mask,vint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}


vint64m8_t test___riscv_vwmulsu(vbool8_t mask,vint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwmulsu(mask,op1,op2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwmulsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
