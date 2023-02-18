/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool64_t test___riscv_vmsle(vbool64_t mask,vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsle(vbool32_t mask,vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsle(vbool16_t mask,vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsle(vbool8_t mask,vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool4_t test___riscv_vmsle(vbool4_t mask,vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool2_t test___riscv_vmsle(vbool2_t mask,vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool1_t test___riscv_vmsle(vbool1_t mask,vint8m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool64_t test___riscv_vmsle(vbool64_t mask,vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsle(vbool32_t mask,vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsle(vbool16_t mask,vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsle(vbool8_t mask,vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool4_t test___riscv_vmsle(vbool4_t mask,vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool2_t test___riscv_vmsle(vbool2_t mask,vint16m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool64_t test___riscv_vmsle(vbool64_t mask,vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsle(vbool32_t mask,vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsle(vbool16_t mask,vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsle(vbool8_t mask,vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool4_t test___riscv_vmsle(vbool4_t mask,vint32m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool64_t test___riscv_vmsle(vbool64_t mask,vint64m1_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsle(vbool32_t mask,vint64m2_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsle(vbool16_t mask,vint64m4_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsle(vbool8_t mask,vint64m8_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmsle(mask,op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmsle\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vmsle\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 4 } } */
