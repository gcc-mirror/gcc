/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwadd_wx(vint16mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint16mf2_t test___riscv_vwadd_wx(vint16mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint16m1_t test___riscv_vwadd_wx(vint16m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint16m2_t test___riscv_vwadd_wx(vint16m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint16m4_t test___riscv_vwadd_wx(vint16m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint16m8_t test___riscv_vwadd_wx(vint16m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint32mf2_t test___riscv_vwadd_wx(vint32mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint32m1_t test___riscv_vwadd_wx(vint32m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint32m2_t test___riscv_vwadd_wx(vint32m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint32m4_t test___riscv_vwadd_wx(vint32m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint32m8_t test___riscv_vwadd_wx(vint32m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint64m1_t test___riscv_vwadd_wx(vint64m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint64m2_t test___riscv_vwadd_wx(vint64m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint64m4_t test___riscv_vwadd_wx(vint64m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint64m8_t test___riscv_vwadd_wx(vint64m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_wx(op1,0xAA,32);
}


vint16mf4_t test___riscv_vwadd_wx(vbool64_t mask,vint16mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint16mf2_t test___riscv_vwadd_wx(vbool32_t mask,vint16mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint16m1_t test___riscv_vwadd_wx(vbool16_t mask,vint16m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint16m2_t test___riscv_vwadd_wx(vbool8_t mask,vint16m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint16m4_t test___riscv_vwadd_wx(vbool4_t mask,vint16m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint16m8_t test___riscv_vwadd_wx(vbool2_t mask,vint16m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint32mf2_t test___riscv_vwadd_wx(vbool64_t mask,vint32mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint32m1_t test___riscv_vwadd_wx(vbool32_t mask,vint32m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint32m2_t test___riscv_vwadd_wx(vbool16_t mask,vint32m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint32m4_t test___riscv_vwadd_wx(vbool8_t mask,vint32m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint32m8_t test___riscv_vwadd_wx(vbool4_t mask,vint32m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint64m1_t test___riscv_vwadd_wx(vbool64_t mask,vint64m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint64m2_t test___riscv_vwadd_wx(vbool32_t mask,vint64m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint64m4_t test___riscv_vwadd_wx(vbool16_t mask,vint64m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}


vint64m8_t test___riscv_vwadd_wx(vbool8_t mask,vint64m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_wx(mask,op1,0xAA,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
