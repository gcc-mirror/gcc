/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vmulhsu(vint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint8mf4_t test___riscv_vmulhsu(vint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint8mf2_t test___riscv_vmulhsu(vint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint8m1_t test___riscv_vmulhsu(vint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint8m2_t test___riscv_vmulhsu(vint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint8m4_t test___riscv_vmulhsu(vint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint8m8_t test___riscv_vmulhsu(vint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint16mf4_t test___riscv_vmulhsu(vint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint16mf2_t test___riscv_vmulhsu(vint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint16m1_t test___riscv_vmulhsu(vint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint16m2_t test___riscv_vmulhsu(vint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint16m4_t test___riscv_vmulhsu(vint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint16m8_t test___riscv_vmulhsu(vint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint32mf2_t test___riscv_vmulhsu(vint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint32m1_t test___riscv_vmulhsu(vint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint32m2_t test___riscv_vmulhsu(vint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint32m4_t test___riscv_vmulhsu(vint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint32m8_t test___riscv_vmulhsu(vint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint64m1_t test___riscv_vmulhsu(vint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint64m2_t test___riscv_vmulhsu(vint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint64m4_t test___riscv_vmulhsu(vint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint64m8_t test___riscv_vmulhsu(vint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu(op1,op2,32);
}


vint8mf8_t test___riscv_vmulhsu(vbool64_t mask,vint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint8mf4_t test___riscv_vmulhsu(vbool32_t mask,vint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint8mf2_t test___riscv_vmulhsu(vbool16_t mask,vint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint8m1_t test___riscv_vmulhsu(vbool8_t mask,vint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint8m2_t test___riscv_vmulhsu(vbool4_t mask,vint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint8m4_t test___riscv_vmulhsu(vbool2_t mask,vint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint8m8_t test___riscv_vmulhsu(vbool1_t mask,vint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint16mf4_t test___riscv_vmulhsu(vbool64_t mask,vint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint16mf2_t test___riscv_vmulhsu(vbool32_t mask,vint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint16m1_t test___riscv_vmulhsu(vbool16_t mask,vint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint16m2_t test___riscv_vmulhsu(vbool8_t mask,vint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint16m4_t test___riscv_vmulhsu(vbool4_t mask,vint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint16m8_t test___riscv_vmulhsu(vbool2_t mask,vint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint32mf2_t test___riscv_vmulhsu(vbool64_t mask,vint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint32m1_t test___riscv_vmulhsu(vbool32_t mask,vint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint32m2_t test___riscv_vmulhsu(vbool16_t mask,vint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint32m4_t test___riscv_vmulhsu(vbool8_t mask,vint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint32m8_t test___riscv_vmulhsu(vbool4_t mask,vint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint64m1_t test___riscv_vmulhsu(vbool64_t mask,vint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint64m2_t test___riscv_vmulhsu(vbool32_t mask,vint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint64m4_t test___riscv_vmulhsu(vbool16_t mask,vint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}


vint64m8_t test___riscv_vmulhsu(vbool8_t mask,vint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu(mask,op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vmulhsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 4 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vmulhsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 4 } } */
