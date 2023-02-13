/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t 
test___riscv_vle8_v_i8mf8(int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8mf8(base,32);
}

vint8mf4_t 
test___riscv_vle8_v_i8mf4(int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8mf4(base,32);
}

vint8mf2_t 
test___riscv_vle8_v_i8mf2(int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8mf2(base,32);
}

vint8m1_t 
test___riscv_vle8_v_i8m1(int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m1(base,32);
}

vint8m2_t 
test___riscv_vle8_v_i8m2(int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m2(base,32);
}

vint8m4_t 
test___riscv_vle8_v_i8m4(int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m4(base,32);
}

vint8m8_t 
test___riscv_vle8_v_i8m8(int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m8(base,32);
}

vuint8mf8_t 
test___riscv_vle8_v_u8mf8(uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8mf8(base,32);
}

vuint8mf4_t 
test___riscv_vle8_v_u8mf4(uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8mf4(base,32);
}

vuint8mf2_t 
test___riscv_vle8_v_u8mf2(uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8mf2(base,32);
}

vuint8m1_t 
test___riscv_vle8_v_u8m1(uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m1(base,32);
}

vuint8m2_t 
test___riscv_vle8_v_u8m2(uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m2(base,32);
}

vuint8m4_t 
test___riscv_vle8_v_u8m4(uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m4(base,32);
}

vuint8m8_t 
test___riscv_vle8_v_u8m8(uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m8(base,32);
}

vint16mf4_t
test___riscv_vle16_v_i16mf4(int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16mf4(base,32);
}

vint16mf2_t
test___riscv_vle16_v_i16mf2(int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16mf2(base,32);
}

vint16m1_t
test___riscv_vle16_v_i16m1(int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m1(base,32);
}

vint16m2_t
test___riscv_vle16_v_i16m2(int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m2(base,32);
}

vint16m4_t
test___riscv_vle16_v_i16m4(int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m4(base,32);
}

vint16m8_t
test___riscv_vle16_v_i16m8(int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m8(base,32);
}

vuint16mf4_t
test___riscv_vle16_v_u16mf4(uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16mf4(base,32);
}

vuint16mf2_t
test___riscv_vle16_v_u16mf2(uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16mf2(base,32);
}

vuint16m1_t
test___riscv_vle16_v_u16m1(uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m1(base,32);
}

vuint16m2_t
test___riscv_vle16_v_u16m2(uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m2(base,32);
}

vuint16m4_t
test___riscv_vle16_v_u16m4(uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m4(base,32);
}

vuint16m8_t
test___riscv_vle16_v_u16m8(uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m8(base,32);
}

vint32mf2_t
test___riscv_vle32_v_i32mf2(int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32mf2(base,32);
}

vint32m1_t
test___riscv_vle32_v_i32m1(int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m1(base,32);
}

vint32m2_t
test___riscv_vle32_v_i32m2(int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m2(base,32);
}

vint32m4_t
test___riscv_vle32_v_i32m4(int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m4(base,32);
}

vint32m8_t
test___riscv_vle32_v_i32m8(int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m8(base,32);
}

vuint32mf2_t
test___riscv_vle32_v_u32mf2(uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32mf2(base,32);
}

vuint32m1_t
test___riscv_vle32_v_u32m1(uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m1(base,32);
}

vuint32m2_t
test___riscv_vle32_v_u32m2(uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m2(base,32);
}

vuint32m4_t
test___riscv_vle32_v_u32m4(uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m4(base,32);
}

vuint32m8_t
test___riscv_vle32_v_u32m8(uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m8(base,32);
}

vfloat32mf2_t
test___riscv_vle32_v_f32mf2(float* base,size_t vl)
{
  return __riscv_vle32_v_f32mf2(base,32);
}

vfloat32m1_t
test___riscv_vle32_v_f32m1(float* base,size_t vl)
{
  return __riscv_vle32_v_f32m1(base,32);
}

vfloat32m2_t
test___riscv_vle32_v_f32m2(float* base,size_t vl)
{
  return __riscv_vle32_v_f32m2(base,32);
}

vfloat32m4_t
test___riscv_vle32_v_f32m4(float* base,size_t vl)
{
  return __riscv_vle32_v_f32m4(base,32);
}

vfloat32m8_t
test___riscv_vle32_v_f32m8(float* base,size_t vl)
{
  return __riscv_vle32_v_f32m8(base,32);
}
vint64m1_t
test___riscv_vle64_v_i64m1(int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m1(base,32);
}

vint64m2_t
test___riscv_vle64_v_i64m2(int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m2(base,32);
}

vint64m4_t
test___riscv_vle64_v_i64m4(int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m4(base,32);
}

vint64m8_t
test___riscv_vle64_v_i64m8(int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m8(base,32);
}

vuint64m1_t
test___riscv_vle64_v_u64m1(uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m1(base,32);
}

vuint64m2_t
test___riscv_vle64_v_u64m2(uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m2(base,32);
}

vuint64m4_t
test___riscv_vle64_v_u64m4(uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m4(base,32);
}

vuint64m8_t
test___riscv_vle64_v_u64m8(uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m8(base,32);
}

vfloat64m1_t
test___riscv_vle64_v_f64m1(double* base,size_t vl)
{
  return __riscv_vle64_v_f64m1(base,32);
}

vfloat64m2_t
test___riscv_vle64_v_f64m2(double* base,size_t vl)
{
  return __riscv_vle64_v_f64m2(base,32);
}

vfloat64m4_t
test___riscv_vle64_v_f64m4(double* base,size_t vl)
{
  return __riscv_vle64_v_f64m4(base,32);
}

vfloat64m8_t
test___riscv_vle64_v_f64m8(double* base,size_t vl)
{
  return __riscv_vle64_v_f64m8(base,32);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\)} 3 } } */

