/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool64_t test___riscv_vmadc(vint8mf8_t op1,vint8mf8_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool32_t test___riscv_vmadc(vint8mf4_t op1,vint8mf4_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool16_t test___riscv_vmadc(vint8mf2_t op1,vint8mf2_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool8_t test___riscv_vmadc(vint8m1_t op1,vint8m1_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool4_t test___riscv_vmadc(vint8m2_t op1,vint8m2_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool2_t test___riscv_vmadc(vint8m4_t op1,vint8m4_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool1_t test___riscv_vmadc(vint8m8_t op1,vint8m8_t op2,vbool1_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool64_t test___riscv_vmadc(vint16mf4_t op1,vint16mf4_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool32_t test___riscv_vmadc(vint16mf2_t op1,vint16mf2_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool16_t test___riscv_vmadc(vint16m1_t op1,vint16m1_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool8_t test___riscv_vmadc(vint16m2_t op1,vint16m2_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool4_t test___riscv_vmadc(vint16m4_t op1,vint16m4_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool2_t test___riscv_vmadc(vint16m8_t op1,vint16m8_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool64_t test___riscv_vmadc(vint32mf2_t op1,vint32mf2_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool32_t test___riscv_vmadc(vint32m1_t op1,vint32m1_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool16_t test___riscv_vmadc(vint32m2_t op1,vint32m2_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool8_t test___riscv_vmadc(vint32m4_t op1,vint32m4_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool4_t test___riscv_vmadc(vint32m8_t op1,vint32m8_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool64_t test___riscv_vmadc(vint64m1_t op1,vint64m1_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool32_t test___riscv_vmadc(vint64m2_t op1,vint64m2_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool16_t test___riscv_vmadc(vint64m4_t op1,vint64m4_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool8_t test___riscv_vmadc(vint64m8_t op1,vint64m8_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool64_t test___riscv_vmadc(vuint8mf8_t op1,vuint8mf8_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool32_t test___riscv_vmadc(vuint8mf4_t op1,vuint8mf4_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool16_t test___riscv_vmadc(vuint8mf2_t op1,vuint8mf2_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool8_t test___riscv_vmadc(vuint8m1_t op1,vuint8m1_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool4_t test___riscv_vmadc(vuint8m2_t op1,vuint8m2_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool2_t test___riscv_vmadc(vuint8m4_t op1,vuint8m4_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool1_t test___riscv_vmadc(vuint8m8_t op1,vuint8m8_t op2,vbool1_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool64_t test___riscv_vmadc(vuint16mf4_t op1,vuint16mf4_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool32_t test___riscv_vmadc(vuint16mf2_t op1,vuint16mf2_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool16_t test___riscv_vmadc(vuint16m1_t op1,vuint16m1_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool8_t test___riscv_vmadc(vuint16m2_t op1,vuint16m2_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool4_t test___riscv_vmadc(vuint16m4_t op1,vuint16m4_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool2_t test___riscv_vmadc(vuint16m8_t op1,vuint16m8_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool64_t test___riscv_vmadc(vuint32mf2_t op1,vuint32mf2_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool32_t test___riscv_vmadc(vuint32m1_t op1,vuint32m1_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool16_t test___riscv_vmadc(vuint32m2_t op1,vuint32m2_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool8_t test___riscv_vmadc(vuint32m4_t op1,vuint32m4_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool4_t test___riscv_vmadc(vuint32m8_t op1,vuint32m8_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool64_t test___riscv_vmadc(vuint64m1_t op1,vuint64m1_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool32_t test___riscv_vmadc(vuint64m2_t op1,vuint64m2_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool16_t test___riscv_vmadc(vuint64m4_t op1,vuint64m4_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}


vbool8_t test___riscv_vmadc(vuint64m8_t op1,vuint64m8_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vmadc(op1,op2,carryin,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vmadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 2 } } */
