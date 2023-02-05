/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint8mf8_t test___riscv_vssubu(vuint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint8mf4_t test___riscv_vssubu(vuint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint8mf2_t test___riscv_vssubu(vuint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint8m1_t test___riscv_vssubu(vuint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint8m2_t test___riscv_vssubu(vuint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint8m4_t test___riscv_vssubu(vuint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint8m8_t test___riscv_vssubu(vuint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint16mf4_t test___riscv_vssubu(vuint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint16mf2_t test___riscv_vssubu(vuint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint16m1_t test___riscv_vssubu(vuint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint16m2_t test___riscv_vssubu(vuint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint16m4_t test___riscv_vssubu(vuint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint16m8_t test___riscv_vssubu(vuint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint32mf2_t test___riscv_vssubu(vuint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint32m1_t test___riscv_vssubu(vuint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint32m2_t test___riscv_vssubu(vuint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint32m4_t test___riscv_vssubu(vuint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint32m8_t test___riscv_vssubu(vuint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint64m1_t test___riscv_vssubu(vuint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint64m2_t test___riscv_vssubu(vuint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint64m4_t test___riscv_vssubu(vuint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint64m8_t test___riscv_vssubu(vuint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu(op1,op2,32);
}


vuint8mf8_t test___riscv_vssubu(vbool64_t mask,vuint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint8mf4_t test___riscv_vssubu(vbool32_t mask,vuint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint8mf2_t test___riscv_vssubu(vbool16_t mask,vuint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint8m1_t test___riscv_vssubu(vbool8_t mask,vuint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint8m2_t test___riscv_vssubu(vbool4_t mask,vuint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint8m4_t test___riscv_vssubu(vbool2_t mask,vuint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint8m8_t test___riscv_vssubu(vbool1_t mask,vuint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint16mf4_t test___riscv_vssubu(vbool64_t mask,vuint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint16mf2_t test___riscv_vssubu(vbool32_t mask,vuint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint16m1_t test___riscv_vssubu(vbool16_t mask,vuint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint16m2_t test___riscv_vssubu(vbool8_t mask,vuint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint16m4_t test___riscv_vssubu(vbool4_t mask,vuint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint16m8_t test___riscv_vssubu(vbool2_t mask,vuint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint32mf2_t test___riscv_vssubu(vbool64_t mask,vuint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint32m1_t test___riscv_vssubu(vbool32_t mask,vuint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint32m2_t test___riscv_vssubu(vbool16_t mask,vuint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint32m4_t test___riscv_vssubu(vbool8_t mask,vuint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint32m8_t test___riscv_vssubu(vbool4_t mask,vuint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint64m1_t test___riscv_vssubu(vbool64_t mask,vuint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint64m2_t test___riscv_vssubu(vbool32_t mask,vuint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint64m4_t test___riscv_vssubu(vbool16_t mask,vuint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}


vuint64m8_t test___riscv_vssubu(vbool8_t mask,vuint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu(mask,op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vssubu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 4 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vssubu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 4 } } */
