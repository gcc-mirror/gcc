/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint8mf8_t test___riscv_vdivu(vuint8mf8_t op1,vuint8mf8_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint8mf4_t test___riscv_vdivu(vuint8mf4_t op1,vuint8mf4_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint8mf2_t test___riscv_vdivu(vuint8mf2_t op1,vuint8mf2_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint8m1_t test___riscv_vdivu(vuint8m1_t op1,vuint8m1_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint8m2_t test___riscv_vdivu(vuint8m2_t op1,vuint8m2_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint8m4_t test___riscv_vdivu(vuint8m4_t op1,vuint8m4_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint8m8_t test___riscv_vdivu(vuint8m8_t op1,vuint8m8_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint16mf4_t test___riscv_vdivu(vuint16mf4_t op1,vuint16mf4_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint16mf2_t test___riscv_vdivu(vuint16mf2_t op1,vuint16mf2_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint16m1_t test___riscv_vdivu(vuint16m1_t op1,vuint16m1_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint16m2_t test___riscv_vdivu(vuint16m2_t op1,vuint16m2_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint16m4_t test___riscv_vdivu(vuint16m4_t op1,vuint16m4_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint16m8_t test___riscv_vdivu(vuint16m8_t op1,vuint16m8_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint32mf2_t test___riscv_vdivu(vuint32mf2_t op1,vuint32mf2_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint32m1_t test___riscv_vdivu(vuint32m1_t op1,vuint32m1_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint32m2_t test___riscv_vdivu(vuint32m2_t op1,vuint32m2_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint32m4_t test___riscv_vdivu(vuint32m4_t op1,vuint32m4_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint32m8_t test___riscv_vdivu(vuint32m8_t op1,vuint32m8_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint64m1_t test___riscv_vdivu(vuint64m1_t op1,vuint64m1_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint64m2_t test___riscv_vdivu(vuint64m2_t op1,vuint64m2_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint64m4_t test___riscv_vdivu(vuint64m4_t op1,vuint64m4_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint64m8_t test___riscv_vdivu(vuint64m8_t op1,vuint64m8_t op2,size_t vl)
{
    return __riscv_vdivu(op1,op2,vl);
}


vuint8mf8_t test___riscv_vdivu(vbool64_t mask,vuint8mf8_t op1,vuint8mf8_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint8mf4_t test___riscv_vdivu(vbool32_t mask,vuint8mf4_t op1,vuint8mf4_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint8mf2_t test___riscv_vdivu(vbool16_t mask,vuint8mf2_t op1,vuint8mf2_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint8m1_t test___riscv_vdivu(vbool8_t mask,vuint8m1_t op1,vuint8m1_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint8m2_t test___riscv_vdivu(vbool4_t mask,vuint8m2_t op1,vuint8m2_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint8m4_t test___riscv_vdivu(vbool2_t mask,vuint8m4_t op1,vuint8m4_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint8m8_t test___riscv_vdivu(vbool1_t mask,vuint8m8_t op1,vuint8m8_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint16mf4_t test___riscv_vdivu(vbool64_t mask,vuint16mf4_t op1,vuint16mf4_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint16mf2_t test___riscv_vdivu(vbool32_t mask,vuint16mf2_t op1,vuint16mf2_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint16m1_t test___riscv_vdivu(vbool16_t mask,vuint16m1_t op1,vuint16m1_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint16m2_t test___riscv_vdivu(vbool8_t mask,vuint16m2_t op1,vuint16m2_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint16m4_t test___riscv_vdivu(vbool4_t mask,vuint16m4_t op1,vuint16m4_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint16m8_t test___riscv_vdivu(vbool2_t mask,vuint16m8_t op1,vuint16m8_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint32mf2_t test___riscv_vdivu(vbool64_t mask,vuint32mf2_t op1,vuint32mf2_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint32m1_t test___riscv_vdivu(vbool32_t mask,vuint32m1_t op1,vuint32m1_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint32m2_t test___riscv_vdivu(vbool16_t mask,vuint32m2_t op1,vuint32m2_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint32m4_t test___riscv_vdivu(vbool8_t mask,vuint32m4_t op1,vuint32m4_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint32m8_t test___riscv_vdivu(vbool4_t mask,vuint32m8_t op1,vuint32m8_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint64m1_t test___riscv_vdivu(vbool64_t mask,vuint64m1_t op1,vuint64m1_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint64m2_t test___riscv_vdivu(vbool32_t mask,vuint64m2_t op1,vuint64m2_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint64m4_t test___riscv_vdivu(vbool16_t mask,vuint64m4_t op1,vuint64m4_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}


vuint64m8_t test___riscv_vdivu(vbool8_t mask,vuint64m8_t op1,vuint64m8_t op2,size_t vl)
{
    return __riscv_vdivu(mask,op1,op2,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vdivu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
