/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vmerge(vint8mf8_t op1,int8_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint8mf4_t test___riscv_vmerge(vint8mf4_t op1,int8_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint8mf2_t test___riscv_vmerge(vint8mf2_t op1,int8_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint8m1_t test___riscv_vmerge(vint8m1_t op1,int8_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint8m2_t test___riscv_vmerge(vint8m2_t op1,int8_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint8m4_t test___riscv_vmerge(vint8m4_t op1,int8_t op2,vbool2_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint8m8_t test___riscv_vmerge(vint8m8_t op1,int8_t op2,vbool1_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint16mf4_t test___riscv_vmerge(vint16mf4_t op1,int16_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint16mf2_t test___riscv_vmerge(vint16mf2_t op1,int16_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint16m1_t test___riscv_vmerge(vint16m1_t op1,int16_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint16m2_t test___riscv_vmerge(vint16m2_t op1,int16_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint16m4_t test___riscv_vmerge(vint16m4_t op1,int16_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint16m8_t test___riscv_vmerge(vint16m8_t op1,int16_t op2,vbool2_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint32mf2_t test___riscv_vmerge(vint32mf2_t op1,int32_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint32m1_t test___riscv_vmerge(vint32m1_t op1,int32_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint32m2_t test___riscv_vmerge(vint32m2_t op1,int32_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint32m4_t test___riscv_vmerge(vint32m4_t op1,int32_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint32m8_t test___riscv_vmerge(vint32m8_t op1,int32_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint64m1_t test___riscv_vmerge(vint64m1_t op1,int64_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint64m2_t test___riscv_vmerge(vint64m2_t op1,int64_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint64m4_t test___riscv_vmerge(vint64m4_t op1,int64_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vint64m8_t test___riscv_vmerge(vint64m8_t op1,int64_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint8mf8_t test___riscv_vmerge(vuint8mf8_t op1,uint8_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint8mf4_t test___riscv_vmerge(vuint8mf4_t op1,uint8_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint8mf2_t test___riscv_vmerge(vuint8mf2_t op1,uint8_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint8m1_t test___riscv_vmerge(vuint8m1_t op1,uint8_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint8m2_t test___riscv_vmerge(vuint8m2_t op1,uint8_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint8m4_t test___riscv_vmerge(vuint8m4_t op1,uint8_t op2,vbool2_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint8m8_t test___riscv_vmerge(vuint8m8_t op1,uint8_t op2,vbool1_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint16mf4_t test___riscv_vmerge(vuint16mf4_t op1,uint16_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint16mf2_t test___riscv_vmerge(vuint16mf2_t op1,uint16_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint16m1_t test___riscv_vmerge(vuint16m1_t op1,uint16_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint16m2_t test___riscv_vmerge(vuint16m2_t op1,uint16_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint16m4_t test___riscv_vmerge(vuint16m4_t op1,uint16_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint16m8_t test___riscv_vmerge(vuint16m8_t op1,uint16_t op2,vbool2_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint32mf2_t test___riscv_vmerge(vuint32mf2_t op1,uint32_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint32m1_t test___riscv_vmerge(vuint32m1_t op1,uint32_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint32m2_t test___riscv_vmerge(vuint32m2_t op1,uint32_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint32m4_t test___riscv_vmerge(vuint32m4_t op1,uint32_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint32m8_t test___riscv_vmerge(vuint32m8_t op1,uint32_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint64m1_t test___riscv_vmerge(vuint64m1_t op1,uint64_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint64m2_t test___riscv_vmerge(vuint64m2_t op1,uint64_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint64m4_t test___riscv_vmerge(vuint64m4_t op1,uint64_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}


vuint64m8_t test___riscv_vmerge(vuint64m8_t op1,uint64_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge(op1,op2,selector,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmerge\.vxm\s+v[0-9]+,\s*[a-x0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 8 } } */
