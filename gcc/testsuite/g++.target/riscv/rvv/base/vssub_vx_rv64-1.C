/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vssub(vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint8mf4_t test___riscv_vssub(vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint8mf2_t test___riscv_vssub(vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint8m1_t test___riscv_vssub(vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint8m2_t test___riscv_vssub(vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint8m4_t test___riscv_vssub(vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint8m8_t test___riscv_vssub(vint8m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint16mf4_t test___riscv_vssub(vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint16mf2_t test___riscv_vssub(vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint16m1_t test___riscv_vssub(vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint16m2_t test___riscv_vssub(vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint16m4_t test___riscv_vssub(vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint16m8_t test___riscv_vssub(vint16m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint32mf2_t test___riscv_vssub(vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint32m1_t test___riscv_vssub(vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint32m2_t test___riscv_vssub(vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint32m4_t test___riscv_vssub(vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint32m8_t test___riscv_vssub(vint32m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint64m1_t test___riscv_vssub(vint64m1_t op1,int64_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint64m2_t test___riscv_vssub(vint64m2_t op1,int64_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint64m4_t test___riscv_vssub(vint64m4_t op1,int64_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint64m8_t test___riscv_vssub(vint64m8_t op1,int64_t op2,size_t vl)
{
    return __riscv_vssub(op1,op2,vl);
}


vint8mf8_t test___riscv_vssub(vbool64_t mask,vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint8mf4_t test___riscv_vssub(vbool32_t mask,vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint8mf2_t test___riscv_vssub(vbool16_t mask,vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint8m1_t test___riscv_vssub(vbool8_t mask,vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint8m2_t test___riscv_vssub(vbool4_t mask,vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint8m4_t test___riscv_vssub(vbool2_t mask,vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint8m8_t test___riscv_vssub(vbool1_t mask,vint8m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint16mf4_t test___riscv_vssub(vbool64_t mask,vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint16mf2_t test___riscv_vssub(vbool32_t mask,vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint16m1_t test___riscv_vssub(vbool16_t mask,vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint16m2_t test___riscv_vssub(vbool8_t mask,vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint16m4_t test___riscv_vssub(vbool4_t mask,vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint16m8_t test___riscv_vssub(vbool2_t mask,vint16m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint32mf2_t test___riscv_vssub(vbool64_t mask,vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint32m1_t test___riscv_vssub(vbool32_t mask,vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint32m2_t test___riscv_vssub(vbool16_t mask,vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint32m4_t test___riscv_vssub(vbool8_t mask,vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint32m8_t test___riscv_vssub(vbool4_t mask,vint32m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint64m1_t test___riscv_vssub(vbool64_t mask,vint64m1_t op1,int64_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint64m2_t test___riscv_vssub(vbool32_t mask,vint64m2_t op1,int64_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint64m4_t test___riscv_vssub(vbool16_t mask,vint64m4_t op1,int64_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}


vint64m8_t test___riscv_vssub(vbool8_t mask,vint64m8_t op1,int64_t op2,size_t vl)
{
    return __riscv_vssub(mask,op1,op2,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vssub\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
