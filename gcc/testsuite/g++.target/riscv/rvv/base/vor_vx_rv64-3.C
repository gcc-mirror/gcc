/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vor(vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint8mf4_t test___riscv_vor(vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint8mf2_t test___riscv_vor(vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint8m1_t test___riscv_vor(vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint8m2_t test___riscv_vor(vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint8m4_t test___riscv_vor(vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint8m8_t test___riscv_vor(vint8m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint16mf4_t test___riscv_vor(vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint16mf2_t test___riscv_vor(vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint16m1_t test___riscv_vor(vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint16m2_t test___riscv_vor(vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint16m4_t test___riscv_vor(vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint16m8_t test___riscv_vor(vint16m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint32mf2_t test___riscv_vor(vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint32m1_t test___riscv_vor(vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint32m2_t test___riscv_vor(vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint32m4_t test___riscv_vor(vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint32m8_t test___riscv_vor(vint32m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint64m1_t test___riscv_vor(vint64m1_t op1,int64_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint64m2_t test___riscv_vor(vint64m2_t op1,int64_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint64m4_t test___riscv_vor(vint64m4_t op1,int64_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint64m8_t test___riscv_vor(vint64m8_t op1,int64_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint8mf8_t test___riscv_vor(vuint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint8mf4_t test___riscv_vor(vuint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint8mf2_t test___riscv_vor(vuint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint8m1_t test___riscv_vor(vuint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint8m2_t test___riscv_vor(vuint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint8m4_t test___riscv_vor(vuint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint8m8_t test___riscv_vor(vuint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint16mf4_t test___riscv_vor(vuint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint16mf2_t test___riscv_vor(vuint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint16m1_t test___riscv_vor(vuint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint16m2_t test___riscv_vor(vuint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint16m4_t test___riscv_vor(vuint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint16m8_t test___riscv_vor(vuint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint32mf2_t test___riscv_vor(vuint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint32m1_t test___riscv_vor(vuint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint32m2_t test___riscv_vor(vuint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint32m4_t test___riscv_vor(vuint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint32m8_t test___riscv_vor(vuint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint64m1_t test___riscv_vor(vuint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint64m2_t test___riscv_vor(vuint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint64m4_t test___riscv_vor(vuint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vuint64m8_t test___riscv_vor(vuint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vor(op1,op2,32);
}


vint8mf8_t test___riscv_vor(vbool64_t mask,vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint8mf4_t test___riscv_vor(vbool32_t mask,vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint8mf2_t test___riscv_vor(vbool16_t mask,vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint8m1_t test___riscv_vor(vbool8_t mask,vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint8m2_t test___riscv_vor(vbool4_t mask,vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint8m4_t test___riscv_vor(vbool2_t mask,vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint8m8_t test___riscv_vor(vbool1_t mask,vint8m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint16mf4_t test___riscv_vor(vbool64_t mask,vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint16mf2_t test___riscv_vor(vbool32_t mask,vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint16m1_t test___riscv_vor(vbool16_t mask,vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint16m2_t test___riscv_vor(vbool8_t mask,vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint16m4_t test___riscv_vor(vbool4_t mask,vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint16m8_t test___riscv_vor(vbool2_t mask,vint16m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint32mf2_t test___riscv_vor(vbool64_t mask,vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint32m1_t test___riscv_vor(vbool32_t mask,vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint32m2_t test___riscv_vor(vbool16_t mask,vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint32m4_t test___riscv_vor(vbool8_t mask,vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint32m8_t test___riscv_vor(vbool4_t mask,vint32m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint64m1_t test___riscv_vor(vbool64_t mask,vint64m1_t op1,int64_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint64m2_t test___riscv_vor(vbool32_t mask,vint64m2_t op1,int64_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint64m4_t test___riscv_vor(vbool16_t mask,vint64m4_t op1,int64_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vint64m8_t test___riscv_vor(vbool8_t mask,vint64m8_t op1,int64_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint8mf8_t test___riscv_vor(vbool64_t mask,vuint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint8mf4_t test___riscv_vor(vbool32_t mask,vuint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint8mf2_t test___riscv_vor(vbool16_t mask,vuint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint8m1_t test___riscv_vor(vbool8_t mask,vuint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint8m2_t test___riscv_vor(vbool4_t mask,vuint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint8m4_t test___riscv_vor(vbool2_t mask,vuint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint8m8_t test___riscv_vor(vbool1_t mask,vuint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint16mf4_t test___riscv_vor(vbool64_t mask,vuint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint16mf2_t test___riscv_vor(vbool32_t mask,vuint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint16m1_t test___riscv_vor(vbool16_t mask,vuint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint16m2_t test___riscv_vor(vbool8_t mask,vuint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint16m4_t test___riscv_vor(vbool4_t mask,vuint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint16m8_t test___riscv_vor(vbool2_t mask,vuint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint32mf2_t test___riscv_vor(vbool64_t mask,vuint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint32m1_t test___riscv_vor(vbool32_t mask,vuint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint32m2_t test___riscv_vor(vbool16_t mask,vuint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint32m4_t test___riscv_vor(vbool8_t mask,vuint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint32m8_t test___riscv_vor(vbool4_t mask,vuint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint64m1_t test___riscv_vor(vbool64_t mask,vuint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint64m2_t test___riscv_vor(vbool32_t mask,vuint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint64m4_t test___riscv_vor(vbool16_t mask,vuint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}


vuint64m8_t test___riscv_vor(vbool8_t mask,vuint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vor(mask,op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vor\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 2 } } */
