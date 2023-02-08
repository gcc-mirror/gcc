/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vadc(vint8mf8_t op1,int8_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint8mf4_t test___riscv_vadc(vint8mf4_t op1,int8_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint8mf2_t test___riscv_vadc(vint8mf2_t op1,int8_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint8m1_t test___riscv_vadc(vint8m1_t op1,int8_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint8m2_t test___riscv_vadc(vint8m2_t op1,int8_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint8m4_t test___riscv_vadc(vint8m4_t op1,int8_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint8m8_t test___riscv_vadc(vint8m8_t op1,int8_t op2,vbool1_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint16mf4_t test___riscv_vadc(vint16mf4_t op1,int16_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint16mf2_t test___riscv_vadc(vint16mf2_t op1,int16_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint16m1_t test___riscv_vadc(vint16m1_t op1,int16_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint16m2_t test___riscv_vadc(vint16m2_t op1,int16_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint16m4_t test___riscv_vadc(vint16m4_t op1,int16_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint16m8_t test___riscv_vadc(vint16m8_t op1,int16_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint32mf2_t test___riscv_vadc(vint32mf2_t op1,int32_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint32m1_t test___riscv_vadc(vint32m1_t op1,int32_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint32m2_t test___riscv_vadc(vint32m2_t op1,int32_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint32m4_t test___riscv_vadc(vint32m4_t op1,int32_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint32m8_t test___riscv_vadc(vint32m8_t op1,int32_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint64m1_t test___riscv_vadc(vint64m1_t op1,int64_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint64m2_t test___riscv_vadc(vint64m2_t op1,int64_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint64m4_t test___riscv_vadc(vint64m4_t op1,int64_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vint64m8_t test___riscv_vadc(vint64m8_t op1,int64_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint8mf8_t test___riscv_vadc(vuint8mf8_t op1,uint8_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint8mf4_t test___riscv_vadc(vuint8mf4_t op1,uint8_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint8mf2_t test___riscv_vadc(vuint8mf2_t op1,uint8_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint8m1_t test___riscv_vadc(vuint8m1_t op1,uint8_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint8m2_t test___riscv_vadc(vuint8m2_t op1,uint8_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint8m4_t test___riscv_vadc(vuint8m4_t op1,uint8_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint8m8_t test___riscv_vadc(vuint8m8_t op1,uint8_t op2,vbool1_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint16mf4_t test___riscv_vadc(vuint16mf4_t op1,uint16_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint16mf2_t test___riscv_vadc(vuint16mf2_t op1,uint16_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint16m1_t test___riscv_vadc(vuint16m1_t op1,uint16_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint16m2_t test___riscv_vadc(vuint16m2_t op1,uint16_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint16m4_t test___riscv_vadc(vuint16m4_t op1,uint16_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint16m8_t test___riscv_vadc(vuint16m8_t op1,uint16_t op2,vbool2_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint32mf2_t test___riscv_vadc(vuint32mf2_t op1,uint32_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint32m1_t test___riscv_vadc(vuint32m1_t op1,uint32_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint32m2_t test___riscv_vadc(vuint32m2_t op1,uint32_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint32m4_t test___riscv_vadc(vuint32m4_t op1,uint32_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint32m8_t test___riscv_vadc(vuint32m8_t op1,uint32_t op2,vbool4_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint64m1_t test___riscv_vadc(vuint64m1_t op1,uint64_t op2,vbool64_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint64m2_t test___riscv_vadc(vuint64m2_t op1,uint64_t op2,vbool32_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint64m4_t test___riscv_vadc(vuint64m4_t op1,uint64_t op2,vbool16_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}


vuint64m8_t test___riscv_vadc(vuint64m8_t op1,uint64_t op2,vbool8_t carryin,size_t vl)
{
    return __riscv_vadc(op1,op2,carryin,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vadc\.vxm\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vadc\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 8 } } */
