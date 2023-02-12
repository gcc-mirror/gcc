/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t
test___riscv_vle8_v_i8mf8_m(vbool64_t mask,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8mf8_m(mask,base,vl);
}

vint8mf4_t
test___riscv_vle8_v_i8mf4_m(vbool32_t mask,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8mf4_m(mask,base,vl);
}

vint8mf2_t
test___riscv_vle8_v_i8mf2_m(vbool16_t mask,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8mf2_m(mask,base,vl);
}

vint8m1_t
test___riscv_vle8_v_i8m1_m(vbool8_t mask,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m1_m(mask,base,vl);
}

vint8m2_t
test___riscv_vle8_v_i8m2_m(vbool4_t mask,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m2_m(mask,base,vl);
}

vint8m4_t
test___riscv_vle8_v_i8m4_m(vbool2_t mask,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m4_m(mask,base,vl);
}

vint8m8_t
test___riscv_vle8_v_i8m8_m(vbool1_t mask,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m8_m(mask,base,vl);
}

vuint8mf8_t
test___riscv_vle8_v_u8mf8_m(vbool64_t mask,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8mf8_m(mask,base,vl);
}

vuint8mf4_t
test___riscv_vle8_v_u8mf4_m(vbool32_t mask,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8mf4_m(mask,base,vl);
}

vuint8mf2_t
test___riscv_vle8_v_u8mf2_m(vbool16_t mask,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8mf2_m(mask,base,vl);
}

vuint8m1_t
test___riscv_vle8_v_u8m1_m(vbool8_t mask,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m1_m(mask,base,vl);
}

vuint8m2_t
test___riscv_vle8_v_u8m2_m(vbool4_t mask,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m2_m(mask,base,vl);
}

vuint8m4_t
test___riscv_vle8_v_u8m4_m(vbool2_t mask,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m4_m(mask,base,vl);
}

vuint8m8_t
test___riscv_vle8_v_u8m8_m(vbool1_t mask,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m8_m(mask,base,vl);
}

vint16mf4_t
test___riscv_vle16_v_i16mf4_m(vbool64_t mask,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16mf4_m(mask,base,vl);
}

vint16mf2_t
test___riscv_vle16_v_i16mf2_m(vbool32_t mask,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16mf2_m(mask,base,vl);
}

vint16m1_t
test___riscv_vle16_v_i16m1_m(vbool16_t mask,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m1_m(mask,base,vl);
}

vint16m2_t
test___riscv_vle16_v_i16m2_m(vbool8_t mask,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m2_m(mask,base,vl);
}

vint16m4_t
test___riscv_vle16_v_i16m4_m(vbool4_t mask,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m4_m(mask,base,vl);
}

vint16m8_t
test___riscv_vle16_v_i16m8_m(vbool2_t mask,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m8_m(mask,base,vl);
}

vuint16mf4_t
test___riscv_vle16_v_u16mf4_m(vbool64_t mask,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16mf4_m(mask,base,vl);
}

vuint16mf2_t
test___riscv_vle16_v_u16mf2_m(vbool32_t mask,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16mf2_m(mask,base,vl);
}

vuint16m1_t
test___riscv_vle16_v_u16m1_m(vbool16_t mask,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m1_m(mask,base,vl);
}

vuint16m2_t
test___riscv_vle16_v_u16m2_m(vbool8_t mask,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m2_m(mask,base,vl);
}

vuint16m4_t
test___riscv_vle16_v_u16m4_m(vbool4_t mask,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m4_m(mask,base,vl);
}

vuint16m8_t
test___riscv_vle16_v_u16m8_m(vbool2_t mask,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m8_m(mask,base,vl);
}

vint32mf2_t
test___riscv_vle32_v_i32mf2_m(vbool64_t mask,int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32mf2_m(mask,base,vl);
}

vint32m1_t
test___riscv_vle32_v_i32m1_m(vbool32_t mask,int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m1_m(mask,base,vl);
}

vint32m2_t
test___riscv_vle32_v_i32m2_m(vbool16_t mask,int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m2_m(mask,base,vl);
}

vint32m4_t
test___riscv_vle32_v_i32m4_m(vbool8_t mask,int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m4_m(mask,base,vl);
}

vint32m8_t
test___riscv_vle32_v_i32m8_m(vbool4_t mask,int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m8_m(mask,base,vl);
}

vuint32mf2_t
test___riscv_vle32_v_u32mf2_m(vbool64_t mask,uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32mf2_m(mask,base,vl);
}

vuint32m1_t
test___riscv_vle32_v_u32m1_m(vbool32_t mask,uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m1_m(mask,base,vl);
}

vuint32m2_t
test___riscv_vle32_v_u32m2_m(vbool16_t mask,uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m2_m(mask,base,vl);
}

vuint32m4_t
test___riscv_vle32_v_u32m4_m(vbool8_t mask,uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m4_m(mask,base,vl);
}

vuint32m8_t
test___riscv_vle32_v_u32m8_m(vbool4_t mask,uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m8_m(mask,base,vl);
}

vfloat32mf2_t
test___riscv_vle32_v_f32mf2_m(vbool64_t mask,float* base,size_t vl)
{
  return __riscv_vle32_v_f32mf2_m(mask,base,vl);
}

vfloat32m1_t
test___riscv_vle32_v_f32m1_m(vbool32_t mask,float* base,size_t vl)
{
  return __riscv_vle32_v_f32m1_m(mask,base,vl);
}

vfloat32m2_t
test___riscv_vle32_v_f32m2_m(vbool16_t mask,float* base,size_t vl)
{
  return __riscv_vle32_v_f32m2_m(mask,base,vl);
}

vfloat32m4_t
test___riscv_vle32_v_f32m4_m(vbool8_t mask,float* base,size_t vl)
{
  return __riscv_vle32_v_f32m4_m(mask,base,vl);
}

vfloat32m8_t
test___riscv_vle32_v_f32m8_m(vbool4_t mask,float* base,size_t vl)
{
  return __riscv_vle32_v_f32m8_m(mask,base,vl);
}

vint64m1_t
test___riscv_vle64_v_i64m1_m(vbool64_t mask,int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m1_m(mask,base,vl);
}

vint64m2_t
test___riscv_vle64_v_i64m2_m(vbool32_t mask,int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m2_m(mask,base,vl);
}

vint64m4_t
test___riscv_vle64_v_i64m4_m(vbool16_t mask,int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m4_m(mask,base,vl);
}

vint64m8_t
test___riscv_vle64_v_i64m8_m(vbool8_t mask,int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m8_m(mask,base,vl);
}

vuint64m1_t
test___riscv_vle64_v_u64m1_m(vbool64_t mask,uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m1_m(mask,base,vl);
}

vuint64m2_t
test___riscv_vle64_v_u64m2_m(vbool32_t mask,uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m2_m(mask,base,vl);
}

vuint64m4_t
test___riscv_vle64_v_u64m4_m(vbool16_t mask,uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m4_m(mask,base,vl);
}

vuint64m8_t
test___riscv_vle64_v_u64m8_m(vbool8_t mask,uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m8_m(mask,base,vl);
}

vfloat64m1_t
test___riscv_vle64_v_f64m1_m(vbool64_t mask,double* base,size_t vl)
{
  return __riscv_vle64_v_f64m1_m(mask,base,vl);
}

vfloat64m2_t
test___riscv_vle64_v_f64m2_m(vbool32_t mask,double* base,size_t vl)
{
  return __riscv_vle64_v_f64m2_m(mask,base,vl);
}

vfloat64m4_t
test___riscv_vle64_v_f64m4_m(vbool16_t mask,double* base,size_t vl)
{
  return __riscv_vle64_v_f64m4_m(mask,base,vl);
}

vfloat64m8_t
test___riscv_vle64_v_f64m8_m(vbool8_t mask,double* base,size_t vl)
{
  return __riscv_vle64_v_f64m8_m(mask,base,vl);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
