/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool64_t test___riscv_vmsne(vbool64_t mask,vint8mf8_t op1,vint8mf8_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsne(vbool32_t mask,vint8mf4_t op1,vint8mf4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsne(vbool16_t mask,vint8mf2_t op1,vint8mf2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsne(vbool8_t mask,vint8m1_t op1,vint8m1_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool4_t test___riscv_vmsne(vbool4_t mask,vint8m2_t op1,vint8m2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool2_t test___riscv_vmsne(vbool2_t mask,vint8m4_t op1,vint8m4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool1_t test___riscv_vmsne(vbool1_t mask,vint8m8_t op1,vint8m8_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool64_t test___riscv_vmsne(vbool64_t mask,vint16mf4_t op1,vint16mf4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsne(vbool32_t mask,vint16mf2_t op1,vint16mf2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsne(vbool16_t mask,vint16m1_t op1,vint16m1_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsne(vbool8_t mask,vint16m2_t op1,vint16m2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool4_t test___riscv_vmsne(vbool4_t mask,vint16m4_t op1,vint16m4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool2_t test___riscv_vmsne(vbool2_t mask,vint16m8_t op1,vint16m8_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool64_t test___riscv_vmsne(vbool64_t mask,vint32mf2_t op1,vint32mf2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsne(vbool32_t mask,vint32m1_t op1,vint32m1_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsne(vbool16_t mask,vint32m2_t op1,vint32m2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsne(vbool8_t mask,vint32m4_t op1,vint32m4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool4_t test___riscv_vmsne(vbool4_t mask,vint32m8_t op1,vint32m8_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool64_t test___riscv_vmsne(vbool64_t mask,vint64m1_t op1,vint64m1_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsne(vbool32_t mask,vint64m2_t op1,vint64m2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsne(vbool16_t mask,vint64m4_t op1,vint64m4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsne(vbool8_t mask,vint64m8_t op1,vint64m8_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool64_t test___riscv_vmsne(vbool64_t mask,vuint8mf8_t op1,vuint8mf8_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsne(vbool32_t mask,vuint8mf4_t op1,vuint8mf4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsne(vbool16_t mask,vuint8mf2_t op1,vuint8mf2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsne(vbool8_t mask,vuint8m1_t op1,vuint8m1_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool4_t test___riscv_vmsne(vbool4_t mask,vuint8m2_t op1,vuint8m2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool2_t test___riscv_vmsne(vbool2_t mask,vuint8m4_t op1,vuint8m4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool1_t test___riscv_vmsne(vbool1_t mask,vuint8m8_t op1,vuint8m8_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool64_t test___riscv_vmsne(vbool64_t mask,vuint16mf4_t op1,vuint16mf4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsne(vbool32_t mask,vuint16mf2_t op1,vuint16mf2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsne(vbool16_t mask,vuint16m1_t op1,vuint16m1_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsne(vbool8_t mask,vuint16m2_t op1,vuint16m2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool4_t test___riscv_vmsne(vbool4_t mask,vuint16m4_t op1,vuint16m4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool2_t test___riscv_vmsne(vbool2_t mask,vuint16m8_t op1,vuint16m8_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool64_t test___riscv_vmsne(vbool64_t mask,vuint32mf2_t op1,vuint32mf2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsne(vbool32_t mask,vuint32m1_t op1,vuint32m1_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsne(vbool16_t mask,vuint32m2_t op1,vuint32m2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsne(vbool8_t mask,vuint32m4_t op1,vuint32m4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool4_t test___riscv_vmsne(vbool4_t mask,vuint32m8_t op1,vuint32m8_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool64_t test___riscv_vmsne(vbool64_t mask,vuint64m1_t op1,vuint64m1_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool32_t test___riscv_vmsne(vbool32_t mask,vuint64m2_t op1,vuint64m2_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool16_t test___riscv_vmsne(vbool16_t mask,vuint64m4_t op1,vuint64m4_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}


vbool8_t test___riscv_vmsne(vbool8_t mask,vuint64m8_t op1,vuint64m8_t op2,size_t vl)
{
    return __riscv_vmsne(mask,op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vmsne\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
