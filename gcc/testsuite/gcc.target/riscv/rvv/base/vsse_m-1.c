/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void
test___riscv_vsse8_v_i8mf8_m(vbool64_t mask,int8_t* base,ptrdiff_t bstride,vint8mf8_t value,size_t vl)
{
  __riscv_vsse8_v_i8mf8_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_i8mf4_m(vbool32_t mask,int8_t* base,ptrdiff_t bstride,vint8mf4_t value,size_t vl)
{
  __riscv_vsse8_v_i8mf4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_i8mf2_m(vbool16_t mask,int8_t* base,ptrdiff_t bstride,vint8mf2_t value,size_t vl)
{
  __riscv_vsse8_v_i8mf2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_i8m1_m(vbool8_t mask,int8_t* base,ptrdiff_t bstride,vint8m1_t value,size_t vl)
{
  __riscv_vsse8_v_i8m1_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_i8m2_m(vbool4_t mask,int8_t* base,ptrdiff_t bstride,vint8m2_t value,size_t vl)
{
  __riscv_vsse8_v_i8m2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_i8m4_m(vbool2_t mask,int8_t* base,ptrdiff_t bstride,vint8m4_t value,size_t vl)
{
  __riscv_vsse8_v_i8m4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_i8m8_m(vbool1_t mask,int8_t* base,ptrdiff_t bstride,vint8m8_t value,size_t vl)
{
  __riscv_vsse8_v_i8m8_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_u8mf8_m(vbool64_t mask,uint8_t* base,ptrdiff_t bstride,vuint8mf8_t value,size_t vl)
{
  __riscv_vsse8_v_u8mf8_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_u8mf4_m(vbool32_t mask,uint8_t* base,ptrdiff_t bstride,vuint8mf4_t value,size_t vl)
{
  __riscv_vsse8_v_u8mf4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_u8mf2_m(vbool16_t mask,uint8_t* base,ptrdiff_t bstride,vuint8mf2_t value,size_t vl)
{
  __riscv_vsse8_v_u8mf2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_u8m1_m(vbool8_t mask,uint8_t* base,ptrdiff_t bstride,vuint8m1_t value,size_t vl)
{
  __riscv_vsse8_v_u8m1_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_u8m2_m(vbool4_t mask,uint8_t* base,ptrdiff_t bstride,vuint8m2_t value,size_t vl)
{
  __riscv_vsse8_v_u8m2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_u8m4_m(vbool2_t mask,uint8_t* base,ptrdiff_t bstride,vuint8m4_t value,size_t vl)
{
  __riscv_vsse8_v_u8m4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse8_v_u8m8_m(vbool1_t mask,uint8_t* base,ptrdiff_t bstride,vuint8m8_t value,size_t vl)
{
  __riscv_vsse8_v_u8m8_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_i16mf4_m(vbool64_t mask,int16_t* base,ptrdiff_t bstride,vint16mf4_t value,size_t vl)
{
  __riscv_vsse16_v_i16mf4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_i16mf2_m(vbool32_t mask,int16_t* base,ptrdiff_t bstride,vint16mf2_t value,size_t vl)
{
  __riscv_vsse16_v_i16mf2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_i16m1_m(vbool16_t mask,int16_t* base,ptrdiff_t bstride,vint16m1_t value,size_t vl)
{
  __riscv_vsse16_v_i16m1_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_i16m2_m(vbool8_t mask,int16_t* base,ptrdiff_t bstride,vint16m2_t value,size_t vl)
{
  __riscv_vsse16_v_i16m2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_i16m4_m(vbool4_t mask,int16_t* base,ptrdiff_t bstride,vint16m4_t value,size_t vl)
{
  __riscv_vsse16_v_i16m4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_i16m8_m(vbool2_t mask,int16_t* base,ptrdiff_t bstride,vint16m8_t value,size_t vl)
{
  __riscv_vsse16_v_i16m8_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_u16mf4_m(vbool64_t mask,uint16_t* base,ptrdiff_t bstride,vuint16mf4_t value,size_t vl)
{
  __riscv_vsse16_v_u16mf4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_u16mf2_m(vbool32_t mask,uint16_t* base,ptrdiff_t bstride,vuint16mf2_t value,size_t vl)
{
  __riscv_vsse16_v_u16mf2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_u16m1_m(vbool16_t mask,uint16_t* base,ptrdiff_t bstride,vuint16m1_t value,size_t vl)
{
  __riscv_vsse16_v_u16m1_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_u16m2_m(vbool8_t mask,uint16_t* base,ptrdiff_t bstride,vuint16m2_t value,size_t vl)
{
  __riscv_vsse16_v_u16m2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_u16m4_m(vbool4_t mask,uint16_t* base,ptrdiff_t bstride,vuint16m4_t value,size_t vl)
{
  __riscv_vsse16_v_u16m4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse16_v_u16m8_m(vbool2_t mask,uint16_t* base,ptrdiff_t bstride,vuint16m8_t value,size_t vl)
{
  __riscv_vsse16_v_u16m8_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_i32mf2_m(vbool64_t mask,int32_t* base,ptrdiff_t bstride,vint32mf2_t value,size_t vl)
{
  __riscv_vsse32_v_i32mf2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_i32m1_m(vbool32_t mask,int32_t* base,ptrdiff_t bstride,vint32m1_t value,size_t vl)
{
  __riscv_vsse32_v_i32m1_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_i32m2_m(vbool16_t mask,int32_t* base,ptrdiff_t bstride,vint32m2_t value,size_t vl)
{
  __riscv_vsse32_v_i32m2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_i32m4_m(vbool8_t mask,int32_t* base,ptrdiff_t bstride,vint32m4_t value,size_t vl)
{
  __riscv_vsse32_v_i32m4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_i32m8_m(vbool4_t mask,int32_t* base,ptrdiff_t bstride,vint32m8_t value,size_t vl)
{
  __riscv_vsse32_v_i32m8_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_u32mf2_m(vbool64_t mask,uint32_t* base,ptrdiff_t bstride,vuint32mf2_t value,size_t vl)
{
  __riscv_vsse32_v_u32mf2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_u32m1_m(vbool32_t mask,uint32_t* base,ptrdiff_t bstride,vuint32m1_t value,size_t vl)
{
  __riscv_vsse32_v_u32m1_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_u32m2_m(vbool16_t mask,uint32_t* base,ptrdiff_t bstride,vuint32m2_t value,size_t vl)
{
  __riscv_vsse32_v_u32m2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_u32m4_m(vbool8_t mask,uint32_t* base,ptrdiff_t bstride,vuint32m4_t value,size_t vl)
{
  __riscv_vsse32_v_u32m4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_u32m8_m(vbool4_t mask,uint32_t* base,ptrdiff_t bstride,vuint32m8_t value,size_t vl)
{
  __riscv_vsse32_v_u32m8_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_f32mf2_m(vbool64_t mask,float* base,ptrdiff_t bstride,vfloat32mf2_t value,size_t vl)
{
  __riscv_vsse32_v_f32mf2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_f32m1_m(vbool32_t mask,float* base,ptrdiff_t bstride,vfloat32m1_t value,size_t vl)
{
  __riscv_vsse32_v_f32m1_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_f32m2_m(vbool16_t mask,float* base,ptrdiff_t bstride,vfloat32m2_t value,size_t vl)
{
  __riscv_vsse32_v_f32m2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_f32m4_m(vbool8_t mask,float* base,ptrdiff_t bstride,vfloat32m4_t value,size_t vl)
{
  __riscv_vsse32_v_f32m4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse32_v_f32m8_m(vbool4_t mask,float* base,ptrdiff_t bstride,vfloat32m8_t value,size_t vl)
{
  __riscv_vsse32_v_f32m8_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_i64m1_m(vbool64_t mask,int64_t* base,ptrdiff_t bstride,vint64m1_t value,size_t vl)
{
  __riscv_vsse64_v_i64m1_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_i64m2_m(vbool32_t mask,int64_t* base,ptrdiff_t bstride,vint64m2_t value,size_t vl)
{
  __riscv_vsse64_v_i64m2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_i64m4_m(vbool16_t mask,int64_t* base,ptrdiff_t bstride,vint64m4_t value,size_t vl)
{
  __riscv_vsse64_v_i64m4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_i64m8_m(vbool8_t mask,int64_t* base,ptrdiff_t bstride,vint64m8_t value,size_t vl)
{
  __riscv_vsse64_v_i64m8_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_u64m1_m(vbool64_t mask,uint64_t* base,ptrdiff_t bstride,vuint64m1_t value,size_t vl)
{
  __riscv_vsse64_v_u64m1_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_u64m2_m(vbool32_t mask,uint64_t* base,ptrdiff_t bstride,vuint64m2_t value,size_t vl)
{
  __riscv_vsse64_v_u64m2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_u64m4_m(vbool16_t mask,uint64_t* base,ptrdiff_t bstride,vuint64m4_t value,size_t vl)
{
  __riscv_vsse64_v_u64m4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_u64m8_m(vbool8_t mask,uint64_t* base,ptrdiff_t bstride,vuint64m8_t value,size_t vl)
{
  __riscv_vsse64_v_u64m8_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_f64m1_m(vbool64_t mask,double* base,ptrdiff_t bstride,vfloat64m1_t value,size_t vl)
{
  __riscv_vsse64_v_f64m1_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_f64m2_m(vbool32_t mask,double* base,ptrdiff_t bstride,vfloat64m2_t value,size_t vl)
{
  __riscv_vsse64_v_f64m2_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_f64m4_m(vbool16_t mask,double* base,ptrdiff_t bstride,vfloat64m4_t value,size_t vl)
{
  __riscv_vsse64_v_f64m4_m(mask,base,bstride,value,vl);
}

void
test___riscv_vsse64_v_f64m8_m(vbool8_t mask,double* base,ptrdiff_t bstride,vfloat64m8_t value,size_t vl)
{
  __riscv_vsse64_v_f64m8_m(mask,base,bstride,value,vl);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+,\s*v0.t} 3 } } */
