/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwadd_vx(vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint16mf2_t test___riscv_vwadd_vx(vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint16m1_t test___riscv_vwadd_vx(vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint16m2_t test___riscv_vwadd_vx(vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint16m4_t test___riscv_vwadd_vx(vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint16m8_t test___riscv_vwadd_vx(vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint32mf2_t test___riscv_vwadd_vx(vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint32m1_t test___riscv_vwadd_vx(vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint32m2_t test___riscv_vwadd_vx(vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint32m4_t test___riscv_vwadd_vx(vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint32m8_t test___riscv_vwadd_vx(vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint64m1_t test___riscv_vwadd_vx(vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint64m2_t test___riscv_vwadd_vx(vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint64m4_t test___riscv_vwadd_vx(vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint64m8_t test___riscv_vwadd_vx(vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_vx(op1,op2,32);
}


vint16mf4_t test___riscv_vwadd_vx(vbool64_t mask,vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint16mf2_t test___riscv_vwadd_vx(vbool32_t mask,vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint16m1_t test___riscv_vwadd_vx(vbool16_t mask,vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint16m2_t test___riscv_vwadd_vx(vbool8_t mask,vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint16m4_t test___riscv_vwadd_vx(vbool4_t mask,vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint16m8_t test___riscv_vwadd_vx(vbool2_t mask,vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint32mf2_t test___riscv_vwadd_vx(vbool64_t mask,vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint32m1_t test___riscv_vwadd_vx(vbool32_t mask,vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint32m2_t test___riscv_vwadd_vx(vbool16_t mask,vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint32m4_t test___riscv_vwadd_vx(vbool8_t mask,vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint32m8_t test___riscv_vwadd_vx(vbool4_t mask,vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint64m1_t test___riscv_vwadd_vx(vbool64_t mask,vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint64m2_t test___riscv_vwadd_vx(vbool32_t mask,vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint64m4_t test___riscv_vwadd_vx(vbool16_t mask,vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}


vint64m8_t test___riscv_vwadd_vx(vbool8_t mask,vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwadd_vx(mask,op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
