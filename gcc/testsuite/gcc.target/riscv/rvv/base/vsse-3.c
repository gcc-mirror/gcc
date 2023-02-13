/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void
test___riscv_vsse8_v_i8mf8(int8_t* base,ptrdiff_t bstride,vint8mf8_t value,size_t vl)
{
  __riscv_vsse8_v_i8mf8(base,bstride,value,32);
}

void
test___riscv_vsse8_v_i8mf4(int8_t* base,ptrdiff_t bstride,vint8mf4_t value,size_t vl)
{
  __riscv_vsse8_v_i8mf4(base,bstride,value,32);
}

void
test___riscv_vsse8_v_i8mf2(int8_t* base,ptrdiff_t bstride,vint8mf2_t value,size_t vl)
{
  __riscv_vsse8_v_i8mf2(base,bstride,value,32);
}

void
test___riscv_vsse8_v_i8m1(int8_t* base,ptrdiff_t bstride,vint8m1_t value,size_t vl)
{
  __riscv_vsse8_v_i8m1(base,bstride,value,32);
}

void
test___riscv_vsse8_v_i8m2(int8_t* base,ptrdiff_t bstride,vint8m2_t value,size_t vl)
{
  __riscv_vsse8_v_i8m2(base,bstride,value,32);
}

void
test___riscv_vsse8_v_i8m4(int8_t* base,ptrdiff_t bstride,vint8m4_t value,size_t vl)
{
  __riscv_vsse8_v_i8m4(base,bstride,value,32);
}

void
test___riscv_vsse8_v_i8m8(int8_t* base,ptrdiff_t bstride,vint8m8_t value,size_t vl)
{
  __riscv_vsse8_v_i8m8(base,bstride,value,32);
}

void
test___riscv_vsse8_v_u8mf8(uint8_t* base,ptrdiff_t bstride,vuint8mf8_t value,size_t vl)
{
  __riscv_vsse8_v_u8mf8(base,bstride,value,32);
}

void
test___riscv_vsse8_v_u8mf4(uint8_t* base,ptrdiff_t bstride,vuint8mf4_t value,size_t vl)
{
  __riscv_vsse8_v_u8mf4(base,bstride,value,32);
}

void
test___riscv_vsse8_v_u8mf2(uint8_t* base,ptrdiff_t bstride,vuint8mf2_t value,size_t vl)
{
  __riscv_vsse8_v_u8mf2(base,bstride,value,32);
}

void
test___riscv_vsse8_v_u8m1(uint8_t* base,ptrdiff_t bstride,vuint8m1_t value,size_t vl)
{
  __riscv_vsse8_v_u8m1(base,bstride,value,32);
}

void
test___riscv_vsse8_v_u8m2(uint8_t* base,ptrdiff_t bstride,vuint8m2_t value,size_t vl)
{
  __riscv_vsse8_v_u8m2(base,bstride,value,32);
}

void
test___riscv_vsse8_v_u8m4(uint8_t* base,ptrdiff_t bstride,vuint8m4_t value,size_t vl)
{
  __riscv_vsse8_v_u8m4(base,bstride,value,32);
}

void
test___riscv_vsse8_v_u8m8(uint8_t* base,ptrdiff_t bstride,vuint8m8_t value,size_t vl)
{
  __riscv_vsse8_v_u8m8(base,bstride,value,32);
}

void
test___riscv_vsse16_v_i16mf4(int16_t* base,ptrdiff_t bstride,vint16mf4_t value,size_t vl)
{
  __riscv_vsse16_v_i16mf4(base,bstride,value,32);
}

void
test___riscv_vsse16_v_i16mf2(int16_t* base,ptrdiff_t bstride,vint16mf2_t value,size_t vl)
{
  __riscv_vsse16_v_i16mf2(base,bstride,value,32);
}

void
test___riscv_vsse16_v_i16m1(int16_t* base,ptrdiff_t bstride,vint16m1_t value,size_t vl)
{
  __riscv_vsse16_v_i16m1(base,bstride,value,32);
}

void
test___riscv_vsse16_v_i16m2(int16_t* base,ptrdiff_t bstride,vint16m2_t value,size_t vl)
{
  __riscv_vsse16_v_i16m2(base,bstride,value,32);
}

void
test___riscv_vsse16_v_i16m4(int16_t* base,ptrdiff_t bstride,vint16m4_t value,size_t vl)
{
  __riscv_vsse16_v_i16m4(base,bstride,value,32);
}

void
test___riscv_vsse16_v_i16m8(int16_t* base,ptrdiff_t bstride,vint16m8_t value,size_t vl)
{
  __riscv_vsse16_v_i16m8(base,bstride,value,32);
}

void
test___riscv_vsse16_v_u16mf4(uint16_t* base,ptrdiff_t bstride,vuint16mf4_t value,size_t vl)
{
  __riscv_vsse16_v_u16mf4(base,bstride,value,32);
}

void
test___riscv_vsse16_v_u16mf2(uint16_t* base,ptrdiff_t bstride,vuint16mf2_t value,size_t vl)
{
  __riscv_vsse16_v_u16mf2(base,bstride,value,32);
}

void
test___riscv_vsse16_v_u16m1(uint16_t* base,ptrdiff_t bstride,vuint16m1_t value,size_t vl)
{
  __riscv_vsse16_v_u16m1(base,bstride,value,32);
}

void
test___riscv_vsse16_v_u16m2(uint16_t* base,ptrdiff_t bstride,vuint16m2_t value,size_t vl)
{
  __riscv_vsse16_v_u16m2(base,bstride,value,32);
}

void
test___riscv_vsse16_v_u16m4(uint16_t* base,ptrdiff_t bstride,vuint16m4_t value,size_t vl)
{
  __riscv_vsse16_v_u16m4(base,bstride,value,32);
}

void
test___riscv_vsse16_v_u16m8(uint16_t* base,ptrdiff_t bstride,vuint16m8_t value,size_t vl)
{
  __riscv_vsse16_v_u16m8(base,bstride,value,32);
}

void
test___riscv_vsse32_v_i32mf2(int32_t* base,ptrdiff_t bstride,vint32mf2_t value,size_t vl)
{
  __riscv_vsse32_v_i32mf2(base,bstride,value,32);
}

void
test___riscv_vsse32_v_i32m1(int32_t* base,ptrdiff_t bstride,vint32m1_t value,size_t vl)
{
  __riscv_vsse32_v_i32m1(base,bstride,value,32);
}

void
test___riscv_vsse32_v_i32m2(int32_t* base,ptrdiff_t bstride,vint32m2_t value,size_t vl)
{
  __riscv_vsse32_v_i32m2(base,bstride,value,32);
}

void
test___riscv_vsse32_v_i32m4(int32_t* base,ptrdiff_t bstride,vint32m4_t value,size_t vl)
{
  __riscv_vsse32_v_i32m4(base,bstride,value,32);
}

void
test___riscv_vsse32_v_i32m8(int32_t* base,ptrdiff_t bstride,vint32m8_t value,size_t vl)
{
  __riscv_vsse32_v_i32m8(base,bstride,value,32);
}

void
test___riscv_vsse32_v_u32mf2(uint32_t* base,ptrdiff_t bstride,vuint32mf2_t value,size_t vl)
{
  __riscv_vsse32_v_u32mf2(base,bstride,value,32);
}

void
test___riscv_vsse32_v_u32m1(uint32_t* base,ptrdiff_t bstride,vuint32m1_t value,size_t vl)
{
  __riscv_vsse32_v_u32m1(base,bstride,value,32);
}

void
test___riscv_vsse32_v_u32m2(uint32_t* base,ptrdiff_t bstride,vuint32m2_t value,size_t vl)
{
  __riscv_vsse32_v_u32m2(base,bstride,value,32);
}

void
test___riscv_vsse32_v_u32m4(uint32_t* base,ptrdiff_t bstride,vuint32m4_t value,size_t vl)
{
  __riscv_vsse32_v_u32m4(base,bstride,value,32);
}

void
test___riscv_vsse32_v_u32m8(uint32_t* base,ptrdiff_t bstride,vuint32m8_t value,size_t vl)
{
  __riscv_vsse32_v_u32m8(base,bstride,value,32);
}

void
test___riscv_vsse32_v_f32mf2(float* base,ptrdiff_t bstride,vfloat32mf2_t value,size_t vl)
{
  __riscv_vsse32_v_f32mf2(base,bstride,value,32);
}

void
test___riscv_vsse32_v_f32m1(float* base,ptrdiff_t bstride,vfloat32m1_t value,size_t vl)
{
  __riscv_vsse32_v_f32m1(base,bstride,value,32);
}

void
test___riscv_vsse32_v_f32m2(float* base,ptrdiff_t bstride,vfloat32m2_t value,size_t vl)
{
  __riscv_vsse32_v_f32m2(base,bstride,value,32);
}

void
test___riscv_vsse32_v_f32m4(float* base,ptrdiff_t bstride,vfloat32m4_t value,size_t vl)
{
  __riscv_vsse32_v_f32m4(base,bstride,value,32);
}

void
test___riscv_vsse32_v_f32m8(float* base,ptrdiff_t bstride,vfloat32m8_t value,size_t vl)
{
  __riscv_vsse32_v_f32m8(base,bstride,value,32);
}

void
test___riscv_vsse64_v_i64m1(int64_t* base,ptrdiff_t bstride,vint64m1_t value,size_t vl)
{
  __riscv_vsse64_v_i64m1(base,bstride,value,32);
}

void
test___riscv_vsse64_v_i64m2(int64_t* base,ptrdiff_t bstride,vint64m2_t value,size_t vl)
{
  __riscv_vsse64_v_i64m2(base,bstride,value,32);
}

void
test___riscv_vsse64_v_i64m4(int64_t* base,ptrdiff_t bstride,vint64m4_t value,size_t vl)
{
  __riscv_vsse64_v_i64m4(base,bstride,value,32);
}

void
test___riscv_vsse64_v_i64m8(int64_t* base,ptrdiff_t bstride,vint64m8_t value,size_t vl)
{
  __riscv_vsse64_v_i64m8(base,bstride,value,32);
}

void
test___riscv_vsse64_v_u64m1(uint64_t* base,ptrdiff_t bstride,vuint64m1_t value,size_t vl)
{
  __riscv_vsse64_v_u64m1(base,bstride,value,32);
}

void
test___riscv_vsse64_v_u64m2(uint64_t* base,ptrdiff_t bstride,vuint64m2_t value,size_t vl)
{
  __riscv_vsse64_v_u64m2(base,bstride,value,32);
}

void
test___riscv_vsse64_v_u64m4(uint64_t* base,ptrdiff_t bstride,vuint64m4_t value,size_t vl)
{
  __riscv_vsse64_v_u64m4(base,bstride,value,32);
}

void
test___riscv_vsse64_v_u64m8(uint64_t* base,ptrdiff_t bstride,vuint64m8_t value,size_t vl)
{
  __riscv_vsse64_v_u64m8(base,bstride,value,32);
}

void
test___riscv_vsse64_v_f64m1(double* base,ptrdiff_t bstride,vfloat64m1_t value,size_t vl)
{
  __riscv_vsse64_v_f64m1(base,bstride,value,32);
}

void
test___riscv_vsse64_v_f64m2(double* base,ptrdiff_t bstride,vfloat64m2_t value,size_t vl)
{
  __riscv_vsse64_v_f64m2(base,bstride,value,32);
}

void
test___riscv_vsse64_v_f64m4(double* base,ptrdiff_t bstride,vfloat64m4_t value,size_t vl)
{
  __riscv_vsse64_v_f64m4(base,bstride,value,32);
}

void
test___riscv_vsse64_v_f64m8(double* base,ptrdiff_t bstride,vfloat64m8_t value,size_t vl)
{
  __riscv_vsse64_v_f64m8(base,bstride,value,32);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vsse8\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vsse16\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vsse32\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsse64\.v\s+v[0-9]+,0\s*\([a-x0-9]+\),\s*[a-x0-9]+\s+} 3 } } */
