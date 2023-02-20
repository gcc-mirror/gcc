/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint16mf4_t test___riscv_vwaddu_wv(vuint16mf4_t op1,vuint8mf8_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint16mf2_t test___riscv_vwaddu_wv(vuint16mf2_t op1,vuint8mf4_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint16m1_t test___riscv_vwaddu_wv(vuint16m1_t op1,vuint8mf2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint16m2_t test___riscv_vwaddu_wv(vuint16m2_t op1,vuint8m1_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint16m4_t test___riscv_vwaddu_wv(vuint16m4_t op1,vuint8m2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint16m8_t test___riscv_vwaddu_wv(vuint16m8_t op1,vuint8m4_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint32mf2_t test___riscv_vwaddu_wv(vuint32mf2_t op1,vuint16mf4_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint32m1_t test___riscv_vwaddu_wv(vuint32m1_t op1,vuint16mf2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint32m2_t test___riscv_vwaddu_wv(vuint32m2_t op1,vuint16m1_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint32m4_t test___riscv_vwaddu_wv(vuint32m4_t op1,vuint16m2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint32m8_t test___riscv_vwaddu_wv(vuint32m8_t op1,vuint16m4_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint64m1_t test___riscv_vwaddu_wv(vuint64m1_t op1,vuint32mf2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint64m2_t test___riscv_vwaddu_wv(vuint64m2_t op1,vuint32m1_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint64m4_t test___riscv_vwaddu_wv(vuint64m4_t op1,vuint32m2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint64m8_t test___riscv_vwaddu_wv(vuint64m8_t op1,vuint32m4_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(op1,op2,32);
}


vuint16mf4_t test___riscv_vwaddu_wv(vbool64_t mask,vuint16mf4_t op1,vuint8mf8_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint16mf2_t test___riscv_vwaddu_wv(vbool32_t mask,vuint16mf2_t op1,vuint8mf4_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint16m1_t test___riscv_vwaddu_wv(vbool16_t mask,vuint16m1_t op1,vuint8mf2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint16m2_t test___riscv_vwaddu_wv(vbool8_t mask,vuint16m2_t op1,vuint8m1_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint16m4_t test___riscv_vwaddu_wv(vbool4_t mask,vuint16m4_t op1,vuint8m2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint16m8_t test___riscv_vwaddu_wv(vbool2_t mask,vuint16m8_t op1,vuint8m4_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint32mf2_t test___riscv_vwaddu_wv(vbool64_t mask,vuint32mf2_t op1,vuint16mf4_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint32m1_t test___riscv_vwaddu_wv(vbool32_t mask,vuint32m1_t op1,vuint16mf2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint32m2_t test___riscv_vwaddu_wv(vbool16_t mask,vuint32m2_t op1,vuint16m1_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint32m4_t test___riscv_vwaddu_wv(vbool8_t mask,vuint32m4_t op1,vuint16m2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint32m8_t test___riscv_vwaddu_wv(vbool4_t mask,vuint32m8_t op1,vuint16m4_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint64m1_t test___riscv_vwaddu_wv(vbool64_t mask,vuint64m1_t op1,vuint32mf2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint64m2_t test___riscv_vwaddu_wv(vbool32_t mask,vuint64m2_t op1,vuint32m1_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint64m4_t test___riscv_vwaddu_wv(vbool16_t mask,vuint64m4_t op1,vuint32m2_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}


vuint64m8_t test___riscv_vwaddu_wv(vbool8_t mask,vuint64m8_t op1,vuint32m4_t op2,size_t vl)
{
    return __riscv_vwaddu_wv(mask,op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwaddu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
