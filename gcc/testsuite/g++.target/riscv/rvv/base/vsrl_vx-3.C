/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint8mf8_t test___riscv_vsrl(vuint8mf8_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint8mf4_t test___riscv_vsrl(vuint8mf4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint8mf2_t test___riscv_vsrl(vuint8mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint8m1_t test___riscv_vsrl(vuint8m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint8m2_t test___riscv_vsrl(vuint8m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint8m4_t test___riscv_vsrl(vuint8m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint8m8_t test___riscv_vsrl(vuint8m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint16mf4_t test___riscv_vsrl(vuint16mf4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint16mf2_t test___riscv_vsrl(vuint16mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint16m1_t test___riscv_vsrl(vuint16m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint16m2_t test___riscv_vsrl(vuint16m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint16m4_t test___riscv_vsrl(vuint16m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint16m8_t test___riscv_vsrl(vuint16m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint32mf2_t test___riscv_vsrl(vuint32mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint32m1_t test___riscv_vsrl(vuint32m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint32m2_t test___riscv_vsrl(vuint32m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint32m4_t test___riscv_vsrl(vuint32m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint32m8_t test___riscv_vsrl(vuint32m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint64m1_t test___riscv_vsrl(vuint64m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint64m2_t test___riscv_vsrl(vuint64m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint64m4_t test___riscv_vsrl(vuint64m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint64m8_t test___riscv_vsrl(vuint64m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(op1,shift,32);
}


vuint8mf8_t test___riscv_vsrl(vbool64_t mask,vuint8mf8_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint8mf4_t test___riscv_vsrl(vbool32_t mask,vuint8mf4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint8mf2_t test___riscv_vsrl(vbool16_t mask,vuint8mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint8m1_t test___riscv_vsrl(vbool8_t mask,vuint8m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint8m2_t test___riscv_vsrl(vbool4_t mask,vuint8m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint8m4_t test___riscv_vsrl(vbool2_t mask,vuint8m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint8m8_t test___riscv_vsrl(vbool1_t mask,vuint8m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint16mf4_t test___riscv_vsrl(vbool64_t mask,vuint16mf4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint16mf2_t test___riscv_vsrl(vbool32_t mask,vuint16mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint16m1_t test___riscv_vsrl(vbool16_t mask,vuint16m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint16m2_t test___riscv_vsrl(vbool8_t mask,vuint16m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint16m4_t test___riscv_vsrl(vbool4_t mask,vuint16m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint16m8_t test___riscv_vsrl(vbool2_t mask,vuint16m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint32mf2_t test___riscv_vsrl(vbool64_t mask,vuint32mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint32m1_t test___riscv_vsrl(vbool32_t mask,vuint32m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint32m2_t test___riscv_vsrl(vbool16_t mask,vuint32m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint32m4_t test___riscv_vsrl(vbool8_t mask,vuint32m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint32m8_t test___riscv_vsrl(vbool4_t mask,vuint32m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint64m1_t test___riscv_vsrl(vbool64_t mask,vuint64m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint64m2_t test___riscv_vsrl(vbool32_t mask,vuint64m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint64m4_t test___riscv_vsrl(vbool16_t mask,vuint64m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}


vuint64m8_t test___riscv_vsrl(vbool8_t mask,vuint64m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vsrl(mask,op1,shift,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsrl\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
