/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void
test___riscv_vsse8(int8_t* base,ptrdiff_t bstride,vint8mf8_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(int8_t* base,ptrdiff_t bstride,vint8mf4_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(int8_t* base,ptrdiff_t bstride,vint8mf2_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(int8_t* base,ptrdiff_t bstride,vint8m1_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(int8_t* base,ptrdiff_t bstride,vint8m2_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(int8_t* base,ptrdiff_t bstride,vint8m4_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(int8_t* base,ptrdiff_t bstride,vint8m8_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(uint8_t* base,ptrdiff_t bstride,vuint8mf8_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(uint8_t* base,ptrdiff_t bstride,vuint8mf4_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(uint8_t* base,ptrdiff_t bstride,vuint8mf2_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(uint8_t* base,ptrdiff_t bstride,vuint8m1_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(uint8_t* base,ptrdiff_t bstride,vuint8m2_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(uint8_t* base,ptrdiff_t bstride,vuint8m4_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(uint8_t* base,ptrdiff_t bstride,vuint8m8_t value,size_t vl)
{
  __riscv_vsse8(base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool64_t mask,int8_t* base,ptrdiff_t bstride,vint8mf8_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool32_t mask,int8_t* base,ptrdiff_t bstride,vint8mf4_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool16_t mask,int8_t* base,ptrdiff_t bstride,vint8mf2_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool8_t mask,int8_t* base,ptrdiff_t bstride,vint8m1_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool4_t mask,int8_t* base,ptrdiff_t bstride,vint8m2_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool2_t mask,int8_t* base,ptrdiff_t bstride,vint8m4_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool1_t mask,int8_t* base,ptrdiff_t bstride,vint8m8_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool64_t mask,uint8_t* base,ptrdiff_t bstride,vuint8mf8_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool32_t mask,uint8_t* base,ptrdiff_t bstride,vuint8mf4_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool16_t mask,uint8_t* base,ptrdiff_t bstride,vuint8mf2_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool8_t mask,uint8_t* base,ptrdiff_t bstride,vuint8m1_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool4_t mask,uint8_t* base,ptrdiff_t bstride,vuint8m2_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool2_t mask,uint8_t* base,ptrdiff_t bstride,vuint8m4_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8(vbool1_t mask,uint8_t* base,ptrdiff_t bstride,vuint8m8_t value,size_t vl)
{
  __riscv_vsse8(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(int16_t* base,ptrdiff_t bstride,vint16mf4_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(int16_t* base,ptrdiff_t bstride,vint16mf2_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(int16_t* base,ptrdiff_t bstride,vint16m1_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(int16_t* base,ptrdiff_t bstride,vint16m2_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(int16_t* base,ptrdiff_t bstride,vint16m4_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(int16_t* base,ptrdiff_t bstride,vint16m8_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(uint16_t* base,ptrdiff_t bstride,vuint16mf4_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(uint16_t* base,ptrdiff_t bstride,vuint16mf2_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(uint16_t* base,ptrdiff_t bstride,vuint16m1_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(uint16_t* base,ptrdiff_t bstride,vuint16m2_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(uint16_t* base,ptrdiff_t bstride,vuint16m4_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(uint16_t* base,ptrdiff_t bstride,vuint16m8_t value,size_t vl)
{
  __riscv_vsse16(base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool64_t mask,int16_t* base,ptrdiff_t bstride,vint16mf4_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool32_t mask,int16_t* base,ptrdiff_t bstride,vint16mf2_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool16_t mask,int16_t* base,ptrdiff_t bstride,vint16m1_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool8_t mask,int16_t* base,ptrdiff_t bstride,vint16m2_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool4_t mask,int16_t* base,ptrdiff_t bstride,vint16m4_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool2_t mask,int16_t* base,ptrdiff_t bstride,vint16m8_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool64_t mask,uint16_t* base,ptrdiff_t bstride,vuint16mf4_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool32_t mask,uint16_t* base,ptrdiff_t bstride,vuint16mf2_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool16_t mask,uint16_t* base,ptrdiff_t bstride,vuint16m1_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool8_t mask,uint16_t* base,ptrdiff_t bstride,vuint16m2_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool4_t mask,uint16_t* base,ptrdiff_t bstride,vuint16m4_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16(vbool2_t mask,uint16_t* base,ptrdiff_t bstride,vuint16m8_t value,size_t vl)
{
  __riscv_vsse16(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(int32_t* base,ptrdiff_t bstride,vint32mf2_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(int32_t* base,ptrdiff_t bstride,vint32m1_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(int32_t* base,ptrdiff_t bstride,vint32m2_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(int32_t* base,ptrdiff_t bstride,vint32m4_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(int32_t* base,ptrdiff_t bstride,vint32m8_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(uint32_t* base,ptrdiff_t bstride,vuint32mf2_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(uint32_t* base,ptrdiff_t bstride,vuint32m1_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(uint32_t* base,ptrdiff_t bstride,vuint32m2_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(uint32_t* base,ptrdiff_t bstride,vuint32m4_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(uint32_t* base,ptrdiff_t bstride,vuint32m8_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(float* base,ptrdiff_t bstride,vfloat32mf2_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(float* base,ptrdiff_t bstride,vfloat32m1_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(float* base,ptrdiff_t bstride,vfloat32m2_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(float* base,ptrdiff_t bstride,vfloat32m4_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(float* base,ptrdiff_t bstride,vfloat32m8_t value,size_t vl)
{
  __riscv_vsse32(base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool64_t mask,int32_t* base,ptrdiff_t bstride,vint32mf2_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool32_t mask,int32_t* base,ptrdiff_t bstride,vint32m1_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool16_t mask,int32_t* base,ptrdiff_t bstride,vint32m2_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool8_t mask,int32_t* base,ptrdiff_t bstride,vint32m4_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool4_t mask,int32_t* base,ptrdiff_t bstride,vint32m8_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool64_t mask,uint32_t* base,ptrdiff_t bstride,vuint32mf2_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool32_t mask,uint32_t* base,ptrdiff_t bstride,vuint32m1_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool16_t mask,uint32_t* base,ptrdiff_t bstride,vuint32m2_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool8_t mask,uint32_t* base,ptrdiff_t bstride,vuint32m4_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool4_t mask,uint32_t* base,ptrdiff_t bstride,vuint32m8_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool64_t mask,float* base,ptrdiff_t bstride,vfloat32mf2_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool32_t mask,float* base,ptrdiff_t bstride,vfloat32m1_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool16_t mask,float* base,ptrdiff_t bstride,vfloat32m2_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool8_t mask,float* base,ptrdiff_t bstride,vfloat32m4_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32(vbool4_t mask,float* base,ptrdiff_t bstride,vfloat32m8_t value,size_t vl)
{
  __riscv_vsse32(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(int64_t* base,ptrdiff_t bstride,vint64m1_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(int64_t* base,ptrdiff_t bstride,vint64m2_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(int64_t* base,ptrdiff_t bstride,vint64m4_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(int64_t* base,ptrdiff_t bstride,vint64m8_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(uint64_t* base,ptrdiff_t bstride,vuint64m1_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(uint64_t* base,ptrdiff_t bstride,vuint64m2_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(uint64_t* base,ptrdiff_t bstride,vuint64m4_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(uint64_t* base,ptrdiff_t bstride,vuint64m8_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(double* base,ptrdiff_t bstride,vfloat64m1_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(double* base,ptrdiff_t bstride,vfloat64m2_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(double* base,ptrdiff_t bstride,vfloat64m4_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(double* base,ptrdiff_t bstride,vfloat64m8_t value,size_t vl)
{
  __riscv_vsse64(base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool64_t mask,int64_t* base,ptrdiff_t bstride,vint64m1_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool32_t mask,int64_t* base,ptrdiff_t bstride,vint64m2_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool16_t mask,int64_t* base,ptrdiff_t bstride,vint64m4_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool8_t mask,int64_t* base,ptrdiff_t bstride,vint64m8_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool64_t mask,uint64_t* base,ptrdiff_t bstride,vuint64m1_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool32_t mask,uint64_t* base,ptrdiff_t bstride,vuint64m2_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool16_t mask,uint64_t* base,ptrdiff_t bstride,vuint64m4_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool8_t mask,uint64_t* base,ptrdiff_t bstride,vuint64m8_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool64_t mask,double* base,ptrdiff_t bstride,vfloat64m1_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool32_t mask,double* base,ptrdiff_t bstride,vfloat64m2_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool16_t mask,double* base,ptrdiff_t bstride,vfloat64m4_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64(vbool8_t mask,double* base,ptrdiff_t bstride,vfloat64m8_t value,size_t vl)
{
  __riscv_vsse64(mask,base,bstride,value,vl);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
