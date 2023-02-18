/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test____riscv_vsbc_tu(vint8mf8_t maskedoff,vint8mf8_t op1,int8_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint8mf4_t test____riscv_vsbc_tu(vint8mf4_t maskedoff,vint8mf4_t op1,int8_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint8mf2_t test____riscv_vsbc_tu(vint8mf2_t maskedoff,vint8mf2_t op1,int8_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint8m1_t test____riscv_vsbc_tu(vint8m1_t maskedoff,vint8m1_t op1,int8_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint8m2_t test____riscv_vsbc_tu(vint8m2_t maskedoff,vint8m2_t op1,int8_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint8m4_t test____riscv_vsbc_tu(vint8m4_t maskedoff,vint8m4_t op1,int8_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint8m8_t test____riscv_vsbc_tu(vint8m8_t maskedoff,vint8m8_t op1,int8_t op2,vbool1_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint16mf4_t test____riscv_vsbc_tu(vint16mf4_t maskedoff,vint16mf4_t op1,int16_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint16mf2_t test____riscv_vsbc_tu(vint16mf2_t maskedoff,vint16mf2_t op1,int16_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint16m1_t test____riscv_vsbc_tu(vint16m1_t maskedoff,vint16m1_t op1,int16_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint16m2_t test____riscv_vsbc_tu(vint16m2_t maskedoff,vint16m2_t op1,int16_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint16m4_t test____riscv_vsbc_tu(vint16m4_t maskedoff,vint16m4_t op1,int16_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint16m8_t test____riscv_vsbc_tu(vint16m8_t maskedoff,vint16m8_t op1,int16_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint32mf2_t test____riscv_vsbc_tu(vint32mf2_t maskedoff,vint32mf2_t op1,int32_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint32m1_t test____riscv_vsbc_tu(vint32m1_t maskedoff,vint32m1_t op1,int32_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint32m2_t test____riscv_vsbc_tu(vint32m2_t maskedoff,vint32m2_t op1,int32_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint32m4_t test____riscv_vsbc_tu(vint32m4_t maskedoff,vint32m4_t op1,int32_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint32m8_t test____riscv_vsbc_tu(vint32m8_t maskedoff,vint32m8_t op1,int32_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint64m1_t test____riscv_vsbc_tu(vint64m1_t maskedoff,vint64m1_t op1,int64_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint64m2_t test____riscv_vsbc_tu(vint64m2_t maskedoff,vint64m2_t op1,int64_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint64m4_t test____riscv_vsbc_tu(vint64m4_t maskedoff,vint64m4_t op1,int64_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vint64m8_t test____riscv_vsbc_tu(vint64m8_t maskedoff,vint64m8_t op1,int64_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint8mf8_t test____riscv_vsbc_tu(vuint8mf8_t maskedoff,vuint8mf8_t op1,uint8_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint8mf4_t test____riscv_vsbc_tu(vuint8mf4_t maskedoff,vuint8mf4_t op1,uint8_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint8mf2_t test____riscv_vsbc_tu(vuint8mf2_t maskedoff,vuint8mf2_t op1,uint8_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint8m1_t test____riscv_vsbc_tu(vuint8m1_t maskedoff,vuint8m1_t op1,uint8_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint8m2_t test____riscv_vsbc_tu(vuint8m2_t maskedoff,vuint8m2_t op1,uint8_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint8m4_t test____riscv_vsbc_tu(vuint8m4_t maskedoff,vuint8m4_t op1,uint8_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint8m8_t test____riscv_vsbc_tu(vuint8m8_t maskedoff,vuint8m8_t op1,uint8_t op2,vbool1_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint16mf4_t test____riscv_vsbc_tu(vuint16mf4_t maskedoff,vuint16mf4_t op1,uint16_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint16mf2_t test____riscv_vsbc_tu(vuint16mf2_t maskedoff,vuint16mf2_t op1,uint16_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint16m1_t test____riscv_vsbc_tu(vuint16m1_t maskedoff,vuint16m1_t op1,uint16_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint16m2_t test____riscv_vsbc_tu(vuint16m2_t maskedoff,vuint16m2_t op1,uint16_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint16m4_t test____riscv_vsbc_tu(vuint16m4_t maskedoff,vuint16m4_t op1,uint16_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint16m8_t test____riscv_vsbc_tu(vuint16m8_t maskedoff,vuint16m8_t op1,uint16_t op2,vbool2_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint32mf2_t test____riscv_vsbc_tu(vuint32mf2_t maskedoff,vuint32mf2_t op1,uint32_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint32m1_t test____riscv_vsbc_tu(vuint32m1_t maskedoff,vuint32m1_t op1,uint32_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint32m2_t test____riscv_vsbc_tu(vuint32m2_t maskedoff,vuint32m2_t op1,uint32_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint32m4_t test____riscv_vsbc_tu(vuint32m4_t maskedoff,vuint32m4_t op1,uint32_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint32m8_t test____riscv_vsbc_tu(vuint32m8_t maskedoff,vuint32m8_t op1,uint32_t op2,vbool4_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint64m1_t test____riscv_vsbc_tu(vuint64m1_t maskedoff,vuint64m1_t op1,uint64_t op2,vbool64_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint64m2_t test____riscv_vsbc_tu(vuint64m2_t maskedoff,vuint64m2_t op1,uint64_t op2,vbool32_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint64m4_t test____riscv_vsbc_tu(vuint64m4_t maskedoff,vuint64m4_t op1,uint64_t op2,vbool16_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}


vuint64m8_t test____riscv_vsbc_tu(vuint64m8_t maskedoff,vuint64m8_t op1,uint64_t op2,vbool8_t borrowin,size_t vl)
{
    return __riscv_vsbc_tu(maskedoff,op1,op2,borrowin,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vsbc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsbc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 8 } } */
