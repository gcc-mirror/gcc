/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint16mf4_t test___riscv_vzext_vf2(vuint8mf8_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint16mf2_t test___riscv_vzext_vf2(vuint8mf4_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint16m1_t test___riscv_vzext_vf2(vuint8mf2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint16m2_t test___riscv_vzext_vf2(vuint8m1_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint16m4_t test___riscv_vzext_vf2(vuint8m2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint16m8_t test___riscv_vzext_vf2(vuint8m4_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint32mf2_t test___riscv_vzext_vf2(vuint16mf4_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint32m1_t test___riscv_vzext_vf2(vuint16mf2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint32m2_t test___riscv_vzext_vf2(vuint16m1_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint32m4_t test___riscv_vzext_vf2(vuint16m2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint32m8_t test___riscv_vzext_vf2(vuint16m4_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint64m1_t test___riscv_vzext_vf2(vuint32mf2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint64m2_t test___riscv_vzext_vf2(vuint32m1_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint64m4_t test___riscv_vzext_vf2(vuint32m2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint64m8_t test___riscv_vzext_vf2(vuint32m4_t op1,size_t vl)
{
    return __riscv_vzext_vf2(op1,vl);
}


vuint16mf4_t test___riscv_vzext_vf2(vbool64_t mask,vuint8mf8_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint16mf2_t test___riscv_vzext_vf2(vbool32_t mask,vuint8mf4_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint16m1_t test___riscv_vzext_vf2(vbool16_t mask,vuint8mf2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint16m2_t test___riscv_vzext_vf2(vbool8_t mask,vuint8m1_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint16m4_t test___riscv_vzext_vf2(vbool4_t mask,vuint8m2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint16m8_t test___riscv_vzext_vf2(vbool2_t mask,vuint8m4_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint32mf2_t test___riscv_vzext_vf2(vbool64_t mask,vuint16mf4_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint32m1_t test___riscv_vzext_vf2(vbool32_t mask,vuint16mf2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint32m2_t test___riscv_vzext_vf2(vbool16_t mask,vuint16m1_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint32m4_t test___riscv_vzext_vf2(vbool8_t mask,vuint16m2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint32m8_t test___riscv_vzext_vf2(vbool4_t mask,vuint16m4_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint64m1_t test___riscv_vzext_vf2(vbool64_t mask,vuint32mf2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint64m2_t test___riscv_vzext_vf2(vbool32_t mask,vuint32m1_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint64m4_t test___riscv_vzext_vf2(vbool16_t mask,vuint32m2_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}


vuint64m8_t test___riscv_vzext_vf2(vbool8_t mask,vuint32m4_t op1,size_t vl)
{
    return __riscv_vzext_vf2(mask,op1,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vzext\.vf2\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
