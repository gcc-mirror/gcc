/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint8mf8_t test___riscv_vnclipu(vuint16mf4_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint8mf4_t test___riscv_vnclipu(vuint16mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint8mf2_t test___riscv_vnclipu(vuint16m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint8m1_t test___riscv_vnclipu(vuint16m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint8m2_t test___riscv_vnclipu(vuint16m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint8m4_t test___riscv_vnclipu(vuint16m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint16mf4_t test___riscv_vnclipu(vuint32mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint16mf2_t test___riscv_vnclipu(vuint32m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint16m1_t test___riscv_vnclipu(vuint32m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint16m2_t test___riscv_vnclipu(vuint32m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint16m4_t test___riscv_vnclipu(vuint32m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint32mf2_t test___riscv_vnclipu(vuint64m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint32m1_t test___riscv_vnclipu(vuint64m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint32m2_t test___riscv_vnclipu(vuint64m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint32m4_t test___riscv_vnclipu(vuint64m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(op1,shift,vl);
}


vuint8mf8_t test___riscv_vnclipu(vbool64_t mask,vuint16mf4_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint8mf4_t test___riscv_vnclipu(vbool32_t mask,vuint16mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint8mf2_t test___riscv_vnclipu(vbool16_t mask,vuint16m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint8m1_t test___riscv_vnclipu(vbool8_t mask,vuint16m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint8m2_t test___riscv_vnclipu(vbool4_t mask,vuint16m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint8m4_t test___riscv_vnclipu(vbool2_t mask,vuint16m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint16mf4_t test___riscv_vnclipu(vbool64_t mask,vuint32mf2_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint16mf2_t test___riscv_vnclipu(vbool32_t mask,vuint32m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint16m1_t test___riscv_vnclipu(vbool16_t mask,vuint32m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint16m2_t test___riscv_vnclipu(vbool8_t mask,vuint32m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint16m4_t test___riscv_vnclipu(vbool4_t mask,vuint32m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint32mf2_t test___riscv_vnclipu(vbool64_t mask,vuint64m1_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint32m1_t test___riscv_vnclipu(vbool32_t mask,vuint64m2_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint32m2_t test___riscv_vnclipu(vbool16_t mask,vuint64m4_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}


vuint32m4_t test___riscv_vnclipu(vbool8_t mask,vuint64m8_t op1,size_t shift,size_t vl)
{
    return __riscv_vnclipu(mask,op1,shift,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vnclipu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
