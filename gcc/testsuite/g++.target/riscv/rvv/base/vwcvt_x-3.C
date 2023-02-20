/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwcvt_x(vint8mf8_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint16mf2_t test___riscv_vwcvt_x(vint8mf4_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint16m1_t test___riscv_vwcvt_x(vint8mf2_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint16m2_t test___riscv_vwcvt_x(vint8m1_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint16m4_t test___riscv_vwcvt_x(vint8m2_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint16m8_t test___riscv_vwcvt_x(vint8m4_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint32mf2_t test___riscv_vwcvt_x(vint16mf4_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint32m1_t test___riscv_vwcvt_x(vint16mf2_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint32m2_t test___riscv_vwcvt_x(vint16m1_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint32m4_t test___riscv_vwcvt_x(vint16m2_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint32m8_t test___riscv_vwcvt_x(vint16m4_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint64m1_t test___riscv_vwcvt_x(vint32mf2_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint64m2_t test___riscv_vwcvt_x(vint32m1_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint64m4_t test___riscv_vwcvt_x(vint32m2_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint64m8_t test___riscv_vwcvt_x(vint32m4_t src,size_t vl)
{
    return __riscv_vwcvt_x(src,32);
}


vint16mf4_t test___riscv_vwcvt_x(vbool64_t mask,vint8mf8_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint16mf2_t test___riscv_vwcvt_x(vbool32_t mask,vint8mf4_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint16m1_t test___riscv_vwcvt_x(vbool16_t mask,vint8mf2_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint16m2_t test___riscv_vwcvt_x(vbool8_t mask,vint8m1_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint16m4_t test___riscv_vwcvt_x(vbool4_t mask,vint8m2_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint16m8_t test___riscv_vwcvt_x(vbool2_t mask,vint8m4_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint32mf2_t test___riscv_vwcvt_x(vbool64_t mask,vint16mf4_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint32m1_t test___riscv_vwcvt_x(vbool32_t mask,vint16mf2_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint32m2_t test___riscv_vwcvt_x(vbool16_t mask,vint16m1_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint32m4_t test___riscv_vwcvt_x(vbool8_t mask,vint16m2_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint32m8_t test___riscv_vwcvt_x(vbool4_t mask,vint16m4_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint64m1_t test___riscv_vwcvt_x(vbool64_t mask,vint32mf2_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint64m2_t test___riscv_vwcvt_x(vbool32_t mask,vint32m1_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint64m4_t test___riscv_vwcvt_x(vbool16_t mask,vint32m2_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}


vint64m8_t test___riscv_vwcvt_x(vbool8_t mask,vint32m4_t src,size_t vl)
{
    return __riscv_vwcvt_x(mask,src,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
