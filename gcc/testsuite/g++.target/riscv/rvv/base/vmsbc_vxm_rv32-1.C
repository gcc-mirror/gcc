/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool64_t test___riscv_vmsbc(vint8mf8_t op1,int8_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool32_t test___riscv_vmsbc(vint8mf4_t op1,int8_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool16_t test___riscv_vmsbc(vint8mf2_t op1,int8_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool8_t test___riscv_vmsbc(vint8m1_t op1,int8_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool4_t test___riscv_vmsbc(vint8m2_t op1,int8_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool2_t test___riscv_vmsbc(vint8m4_t op1,int8_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool1_t test___riscv_vmsbc(vint8m8_t op1,int8_t op2,vbool1_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool64_t test___riscv_vmsbc(vint16mf4_t op1,int16_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool32_t test___riscv_vmsbc(vint16mf2_t op1,int16_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool16_t test___riscv_vmsbc(vint16m1_t op1,int16_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool8_t test___riscv_vmsbc(vint16m2_t op1,int16_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool4_t test___riscv_vmsbc(vint16m4_t op1,int16_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool2_t test___riscv_vmsbc(vint16m8_t op1,int16_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool64_t test___riscv_vmsbc(vint32mf2_t op1,int32_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool32_t test___riscv_vmsbc(vint32m1_t op1,int32_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool16_t test___riscv_vmsbc(vint32m2_t op1,int32_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool8_t test___riscv_vmsbc(vint32m4_t op1,int32_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool4_t test___riscv_vmsbc(vint32m8_t op1,int32_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool64_t test___riscv_vmsbc(vint64m1_t op1,int64_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool32_t test___riscv_vmsbc(vint64m2_t op1,int64_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool16_t test___riscv_vmsbc(vint64m4_t op1,int64_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool8_t test___riscv_vmsbc(vint64m8_t op1,int64_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool64_t test___riscv_vmsbc(vuint8mf8_t op1,uint8_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool32_t test___riscv_vmsbc(vuint8mf4_t op1,uint8_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool16_t test___riscv_vmsbc(vuint8mf2_t op1,uint8_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool8_t test___riscv_vmsbc(vuint8m1_t op1,uint8_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool4_t test___riscv_vmsbc(vuint8m2_t op1,uint8_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool2_t test___riscv_vmsbc(vuint8m4_t op1,uint8_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool1_t test___riscv_vmsbc(vuint8m8_t op1,uint8_t op2,vbool1_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool64_t test___riscv_vmsbc(vuint16mf4_t op1,uint16_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool32_t test___riscv_vmsbc(vuint16mf2_t op1,uint16_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool16_t test___riscv_vmsbc(vuint16m1_t op1,uint16_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool8_t test___riscv_vmsbc(vuint16m2_t op1,uint16_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool4_t test___riscv_vmsbc(vuint16m4_t op1,uint16_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool2_t test___riscv_vmsbc(vuint16m8_t op1,uint16_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool64_t test___riscv_vmsbc(vuint32mf2_t op1,uint32_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool32_t test___riscv_vmsbc(vuint32m1_t op1,uint32_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool16_t test___riscv_vmsbc(vuint32m2_t op1,uint32_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool8_t test___riscv_vmsbc(vuint32m4_t op1,uint32_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool4_t test___riscv_vmsbc(vuint32m8_t op1,uint32_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool64_t test___riscv_vmsbc(vuint64m1_t op1,uint64_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool32_t test___riscv_vmsbc(vuint64m2_t op1,uint64_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool16_t test___riscv_vmsbc(vuint64m4_t op1,uint64_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}


vbool8_t test___riscv_vmsbc(vuint64m8_t op1,uint64_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vmsbc(op1,op2,borrowin,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vmsbc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 8 } } */
