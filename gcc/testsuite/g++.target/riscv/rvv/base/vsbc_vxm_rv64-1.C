/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vsbc(vint8mf8_t op1,int8_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint8mf4_t test___riscv_vsbc(vint8mf4_t op1,int8_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint8mf2_t test___riscv_vsbc(vint8mf2_t op1,int8_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint8m1_t test___riscv_vsbc(vint8m1_t op1,int8_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint8m2_t test___riscv_vsbc(vint8m2_t op1,int8_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint8m4_t test___riscv_vsbc(vint8m4_t op1,int8_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint8m8_t test___riscv_vsbc(vint8m8_t op1,int8_t op2,vbool1_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint16mf4_t test___riscv_vsbc(vint16mf4_t op1,int16_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint16mf2_t test___riscv_vsbc(vint16mf2_t op1,int16_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint16m1_t test___riscv_vsbc(vint16m1_t op1,int16_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint16m2_t test___riscv_vsbc(vint16m2_t op1,int16_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint16m4_t test___riscv_vsbc(vint16m4_t op1,int16_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint16m8_t test___riscv_vsbc(vint16m8_t op1,int16_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint32mf2_t test___riscv_vsbc(vint32mf2_t op1,int32_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint32m1_t test___riscv_vsbc(vint32m1_t op1,int32_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint32m2_t test___riscv_vsbc(vint32m2_t op1,int32_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint32m4_t test___riscv_vsbc(vint32m4_t op1,int32_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint32m8_t test___riscv_vsbc(vint32m8_t op1,int32_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint64m1_t test___riscv_vsbc(vint64m1_t op1,int64_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint64m2_t test___riscv_vsbc(vint64m2_t op1,int64_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint64m4_t test___riscv_vsbc(vint64m4_t op1,int64_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vint64m8_t test___riscv_vsbc(vint64m8_t op1,int64_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint8mf8_t test___riscv_vsbc(vuint8mf8_t op1,uint8_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint8mf4_t test___riscv_vsbc(vuint8mf4_t op1,uint8_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint8mf2_t test___riscv_vsbc(vuint8mf2_t op1,uint8_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint8m1_t test___riscv_vsbc(vuint8m1_t op1,uint8_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint8m2_t test___riscv_vsbc(vuint8m2_t op1,uint8_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint8m4_t test___riscv_vsbc(vuint8m4_t op1,uint8_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint8m8_t test___riscv_vsbc(vuint8m8_t op1,uint8_t op2,vbool1_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint16mf4_t test___riscv_vsbc(vuint16mf4_t op1,uint16_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint16mf2_t test___riscv_vsbc(vuint16mf2_t op1,uint16_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint16m1_t test___riscv_vsbc(vuint16m1_t op1,uint16_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint16m2_t test___riscv_vsbc(vuint16m2_t op1,uint16_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint16m4_t test___riscv_vsbc(vuint16m4_t op1,uint16_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint16m8_t test___riscv_vsbc(vuint16m8_t op1,uint16_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint32mf2_t test___riscv_vsbc(vuint32mf2_t op1,uint32_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint32m1_t test___riscv_vsbc(vuint32m1_t op1,uint32_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint32m2_t test___riscv_vsbc(vuint32m2_t op1,uint32_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint32m4_t test___riscv_vsbc(vuint32m4_t op1,uint32_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint32m8_t test___riscv_vsbc(vuint32m8_t op1,uint32_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint64m1_t test___riscv_vsbc(vuint64m1_t op1,uint64_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint64m2_t test___riscv_vsbc(vuint64m2_t op1,uint64_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint64m4_t test___riscv_vsbc(vuint64m4_t op1,uint64_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}


vuint64m8_t test___riscv_vsbc(vuint64m8_t op1,uint64_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc(op1,op2,borrowin,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
