/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test____riscv_vsbc_tu(vint8mf8_t maskedoff,vint8mf8_t op1,int8_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint8mf4_t test____riscv_vsbc_tu(vint8mf4_t maskedoff,vint8mf4_t op1,int8_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint8mf2_t test____riscv_vsbc_tu(vint8mf2_t maskedoff,vint8mf2_t op1,int8_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint8m1_t test____riscv_vsbc_tu(vint8m1_t maskedoff,vint8m1_t op1,int8_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint8m2_t test____riscv_vsbc_tu(vint8m2_t maskedoff,vint8m2_t op1,int8_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint8m4_t test____riscv_vsbc_tu(vint8m4_t maskedoff,vint8m4_t op1,int8_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint8m8_t test____riscv_vsbc_tu(vint8m8_t maskedoff,vint8m8_t op1,int8_t op2,vbool1_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint16mf4_t test____riscv_vsbc_tu(vint16mf4_t maskedoff,vint16mf4_t op1,int16_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint16mf2_t test____riscv_vsbc_tu(vint16mf2_t maskedoff,vint16mf2_t op1,int16_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint16m1_t test____riscv_vsbc_tu(vint16m1_t maskedoff,vint16m1_t op1,int16_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint16m2_t test____riscv_vsbc_tu(vint16m2_t maskedoff,vint16m2_t op1,int16_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint16m4_t test____riscv_vsbc_tu(vint16m4_t maskedoff,vint16m4_t op1,int16_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint16m8_t test____riscv_vsbc_tu(vint16m8_t maskedoff,vint16m8_t op1,int16_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint32mf2_t test____riscv_vsbc_tu(vint32mf2_t maskedoff,vint32mf2_t op1,int32_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint32m1_t test____riscv_vsbc_tu(vint32m1_t maskedoff,vint32m1_t op1,int32_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint32m2_t test____riscv_vsbc_tu(vint32m2_t maskedoff,vint32m2_t op1,int32_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint32m4_t test____riscv_vsbc_tu(vint32m4_t maskedoff,vint32m4_t op1,int32_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint32m8_t test____riscv_vsbc_tu(vint32m8_t maskedoff,vint32m8_t op1,int32_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint64m1_t test____riscv_vsbc_tu(vint64m1_t maskedoff,vint64m1_t op1,int64_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint64m2_t test____riscv_vsbc_tu(vint64m2_t maskedoff,vint64m2_t op1,int64_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint64m4_t test____riscv_vsbc_tu(vint64m4_t maskedoff,vint64m4_t op1,int64_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vint64m8_t test____riscv_vsbc_tu(vint64m8_t maskedoff,vint64m8_t op1,int64_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint8mf8_t test____riscv_vsbc_tu(vuint8mf8_t maskedoff,vuint8mf8_t op1,uint8_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint8mf4_t test____riscv_vsbc_tu(vuint8mf4_t maskedoff,vuint8mf4_t op1,uint8_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint8mf2_t test____riscv_vsbc_tu(vuint8mf2_t maskedoff,vuint8mf2_t op1,uint8_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint8m1_t test____riscv_vsbc_tu(vuint8m1_t maskedoff,vuint8m1_t op1,uint8_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint8m2_t test____riscv_vsbc_tu(vuint8m2_t maskedoff,vuint8m2_t op1,uint8_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint8m4_t test____riscv_vsbc_tu(vuint8m4_t maskedoff,vuint8m4_t op1,uint8_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint8m8_t test____riscv_vsbc_tu(vuint8m8_t maskedoff,vuint8m8_t op1,uint8_t op2,vbool1_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint16mf4_t test____riscv_vsbc_tu(vuint16mf4_t maskedoff,vuint16mf4_t op1,uint16_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint16mf2_t test____riscv_vsbc_tu(vuint16mf2_t maskedoff,vuint16mf2_t op1,uint16_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint16m1_t test____riscv_vsbc_tu(vuint16m1_t maskedoff,vuint16m1_t op1,uint16_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint16m2_t test____riscv_vsbc_tu(vuint16m2_t maskedoff,vuint16m2_t op1,uint16_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint16m4_t test____riscv_vsbc_tu(vuint16m4_t maskedoff,vuint16m4_t op1,uint16_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint16m8_t test____riscv_vsbc_tu(vuint16m8_t maskedoff,vuint16m8_t op1,uint16_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint32mf2_t test____riscv_vsbc_tu(vuint32mf2_t maskedoff,vuint32mf2_t op1,uint32_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint32m1_t test____riscv_vsbc_tu(vuint32m1_t maskedoff,vuint32m1_t op1,uint32_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint32m2_t test____riscv_vsbc_tu(vuint32m2_t maskedoff,vuint32m2_t op1,uint32_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint32m4_t test____riscv_vsbc_tu(vuint32m4_t maskedoff,vuint32m4_t op1,uint32_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint32m8_t test____riscv_vsbc_tu(vuint32m8_t maskedoff,vuint32m8_t op1,uint32_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint64m1_t test____riscv_vsbc_tu(vuint64m1_t maskedoff,vuint64m1_t op1,uint64_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint64m2_t test____riscv_vsbc_tu(vuint64m2_t maskedoff,vuint64m2_t op1,uint64_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint64m4_t test____riscv_vsbc_tu(vuint64m4_t maskedoff,vuint64m4_t op1,uint64_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}


vuint64m8_t test____riscv_vsbc_tu(vuint64m8_t maskedoff,vuint64m8_t op1,uint64_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
