/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vmul(vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint8mf4_t test___riscv_vmul(vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint8mf2_t test___riscv_vmul(vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint8m1_t test___riscv_vmul(vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint8m2_t test___riscv_vmul(vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint8m4_t test___riscv_vmul(vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint8m8_t test___riscv_vmul(vint8m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint16mf4_t test___riscv_vmul(vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint16mf2_t test___riscv_vmul(vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint16m1_t test___riscv_vmul(vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint16m2_t test___riscv_vmul(vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint16m4_t test___riscv_vmul(vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint16m8_t test___riscv_vmul(vint16m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint32mf2_t test___riscv_vmul(vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint32m1_t test___riscv_vmul(vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint32m2_t test___riscv_vmul(vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint32m4_t test___riscv_vmul(vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint32m8_t test___riscv_vmul(vint32m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint64m1_t test___riscv_vmul(vint64m1_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint64m2_t test___riscv_vmul(vint64m2_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint64m4_t test___riscv_vmul(vint64m4_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint64m8_t test___riscv_vmul(vint64m8_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint8mf8_t test___riscv_vmul(vuint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint8mf4_t test___riscv_vmul(vuint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint8mf2_t test___riscv_vmul(vuint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint8m1_t test___riscv_vmul(vuint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint8m2_t test___riscv_vmul(vuint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint8m4_t test___riscv_vmul(vuint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint8m8_t test___riscv_vmul(vuint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint16mf4_t test___riscv_vmul(vuint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint16mf2_t test___riscv_vmul(vuint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint16m1_t test___riscv_vmul(vuint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint16m2_t test___riscv_vmul(vuint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint16m4_t test___riscv_vmul(vuint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint16m8_t test___riscv_vmul(vuint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint32mf2_t test___riscv_vmul(vuint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint32m1_t test___riscv_vmul(vuint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint32m2_t test___riscv_vmul(vuint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint32m4_t test___riscv_vmul(vuint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint32m8_t test___riscv_vmul(vuint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint64m1_t test___riscv_vmul(vuint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint64m2_t test___riscv_vmul(vuint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint64m4_t test___riscv_vmul(vuint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vuint64m8_t test___riscv_vmul(vuint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmul(op1,op2,31);
}


vint8mf8_t test___riscv_vmul(vbool64_t mask,vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint8mf4_t test___riscv_vmul(vbool32_t mask,vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint8mf2_t test___riscv_vmul(vbool16_t mask,vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint8m1_t test___riscv_vmul(vbool8_t mask,vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint8m2_t test___riscv_vmul(vbool4_t mask,vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint8m4_t test___riscv_vmul(vbool2_t mask,vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint8m8_t test___riscv_vmul(vbool1_t mask,vint8m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint16mf4_t test___riscv_vmul(vbool64_t mask,vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint16mf2_t test___riscv_vmul(vbool32_t mask,vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint16m1_t test___riscv_vmul(vbool16_t mask,vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint16m2_t test___riscv_vmul(vbool8_t mask,vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint16m4_t test___riscv_vmul(vbool4_t mask,vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint16m8_t test___riscv_vmul(vbool2_t mask,vint16m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint32mf2_t test___riscv_vmul(vbool64_t mask,vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint32m1_t test___riscv_vmul(vbool32_t mask,vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint32m2_t test___riscv_vmul(vbool16_t mask,vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint32m4_t test___riscv_vmul(vbool8_t mask,vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint32m8_t test___riscv_vmul(vbool4_t mask,vint32m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint64m1_t test___riscv_vmul(vbool64_t mask,vint64m1_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint64m2_t test___riscv_vmul(vbool32_t mask,vint64m2_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint64m4_t test___riscv_vmul(vbool16_t mask,vint64m4_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vint64m8_t test___riscv_vmul(vbool8_t mask,vint64m8_t op1,int64_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint8mf8_t test___riscv_vmul(vbool64_t mask,vuint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint8mf4_t test___riscv_vmul(vbool32_t mask,vuint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint8mf2_t test___riscv_vmul(vbool16_t mask,vuint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint8m1_t test___riscv_vmul(vbool8_t mask,vuint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint8m2_t test___riscv_vmul(vbool4_t mask,vuint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint8m4_t test___riscv_vmul(vbool2_t mask,vuint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint8m8_t test___riscv_vmul(vbool1_t mask,vuint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint16mf4_t test___riscv_vmul(vbool64_t mask,vuint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint16mf2_t test___riscv_vmul(vbool32_t mask,vuint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint16m1_t test___riscv_vmul(vbool16_t mask,vuint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint16m2_t test___riscv_vmul(vbool8_t mask,vuint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint16m4_t test___riscv_vmul(vbool4_t mask,vuint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint16m8_t test___riscv_vmul(vbool2_t mask,vuint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint32mf2_t test___riscv_vmul(vbool64_t mask,vuint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint32m1_t test___riscv_vmul(vbool32_t mask,vuint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint32m2_t test___riscv_vmul(vbool16_t mask,vuint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint32m4_t test___riscv_vmul(vbool8_t mask,vuint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint32m8_t test___riscv_vmul(vbool4_t mask,vuint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint64m1_t test___riscv_vmul(vbool64_t mask,vuint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint64m2_t test___riscv_vmul(vbool32_t mask,vuint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint64m4_t test___riscv_vmul(vbool16_t mask,vuint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}


vuint64m8_t test___riscv_vmul(vbool8_t mask,vuint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmul(mask,op1,op2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 8 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 8 } } */
