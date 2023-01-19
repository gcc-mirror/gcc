/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t
test___riscv_vle8_v_i8mf8_tum(vbool64_t mask,vint8mf8_t merge,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8mf8_tum(mask,merge,base,31);
}

vint8mf4_t
test___riscv_vle8_v_i8mf4_tum(vbool32_t mask,vint8mf4_t merge,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8mf4_tum(mask,merge,base,31);
}

vint8mf2_t
test___riscv_vle8_v_i8mf2_tum(vbool16_t mask,vint8mf2_t merge,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8mf2_tum(mask,merge,base,31);
}

vint8m1_t
test___riscv_vle8_v_i8m1_tum(vbool8_t mask,vint8m1_t merge,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m1_tum(mask,merge,base,31);
}

vint8m2_t
test___riscv_vle8_v_i8m2_tum(vbool4_t mask,vint8m2_t merge,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m2_tum(mask,merge,base,31);
}

vint8m4_t
test___riscv_vle8_v_i8m4_tum(vbool2_t mask,vint8m4_t merge,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m4_tum(mask,merge,base,31);
}

vint8m8_t
test___riscv_vle8_v_i8m8_tum(vbool1_t mask,vint8m8_t merge,int8_t* base,size_t vl)
{
  return __riscv_vle8_v_i8m8_tum(mask,merge,base,31);
}

vuint8mf8_t
test___riscv_vle8_v_u8mf8_tum(vbool64_t mask,vuint8mf8_t merge,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8mf8_tum(mask,merge,base,31);
}

vuint8mf4_t
test___riscv_vle8_v_u8mf4_tum(vbool32_t mask,vuint8mf4_t merge,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8mf4_tum(mask,merge,base,31);
}

vuint8mf2_t
test___riscv_vle8_v_u8mf2_tum(vbool16_t mask,vuint8mf2_t merge,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8mf2_tum(mask,merge,base,31);
}

vuint8m1_t
test___riscv_vle8_v_u8m1_tum(vbool8_t mask,vuint8m1_t merge,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m1_tum(mask,merge,base,31);
}

vuint8m2_t
test___riscv_vle8_v_u8m2_tum(vbool4_t mask,vuint8m2_t merge,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m2_tum(mask,merge,base,31);
}

vuint8m4_t
test___riscv_vle8_v_u8m4_tum(vbool2_t mask,vuint8m4_t merge,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m4_tum(mask,merge,base,31);
}

vuint8m8_t
test___riscv_vle8_v_u8m8_tum(vbool1_t mask,vuint8m8_t merge,uint8_t* base,size_t vl)
{
  return __riscv_vle8_v_u8m8_tum(mask,merge,base,31);
}

vint16mf4_t
test___riscv_vle16_v_i16mf4_tum(vbool64_t mask,vint16mf4_t merge,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16mf4_tum(mask,merge,base,31);
}

vint16mf2_t
test___riscv_vle16_v_i16mf2_tum(vbool32_t mask,vint16mf2_t merge,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16mf2_tum(mask,merge,base,31);
}

vint16m1_t
test___riscv_vle16_v_i16m1_tum(vbool16_t mask,vint16m1_t merge,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m1_tum(mask,merge,base,31);
}

vint16m2_t
test___riscv_vle16_v_i16m2_tum(vbool8_t mask,vint16m2_t merge,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m2_tum(mask,merge,base,31);
}

vint16m4_t
test___riscv_vle16_v_i16m4_tum(vbool4_t mask,vint16m4_t merge,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m4_tum(mask,merge,base,31);
}

vint16m8_t
test___riscv_vle16_v_i16m8_tum(vbool2_t mask,vint16m8_t merge,int16_t* base,size_t vl)
{
  return __riscv_vle16_v_i16m8_tum(mask,merge,base,31);
}

vuint16mf4_t
test___riscv_vle16_v_u16mf4_tum(vbool64_t mask,vuint16mf4_t merge,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16mf4_tum(mask,merge,base,31);
}

vuint16mf2_t
test___riscv_vle16_v_u16mf2_tum(vbool32_t mask,vuint16mf2_t merge,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16mf2_tum(mask,merge,base,31);
}

vuint16m1_t
test___riscv_vle16_v_u16m1_tum(vbool16_t mask,vuint16m1_t merge,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m1_tum(mask,merge,base,31);
}

vuint16m2_t
test___riscv_vle16_v_u16m2_tum(vbool8_t mask,vuint16m2_t merge,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m2_tum(mask,merge,base,31);
}

vuint16m4_t
test___riscv_vle16_v_u16m4_tum(vbool4_t mask,vuint16m4_t merge,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m4_tum(mask,merge,base,31);
}

vuint16m8_t
test___riscv_vle16_v_u16m8_tum(vbool2_t mask,vuint16m8_t merge,uint16_t* base,size_t vl)
{
  return __riscv_vle16_v_u16m8_tum(mask,merge,base,31);
}

vint32mf2_t
test___riscv_vle32_v_i32mf2_tum(vbool64_t mask,vint32mf2_t merge,int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32mf2_tum(mask,merge,base,31);
}

vint32m1_t
test___riscv_vle32_v_i32m1_tum(vbool32_t mask,vint32m1_t merge,int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m1_tum(mask,merge,base,31);
}

vint32m2_t
test___riscv_vle32_v_i32m2_tum(vbool16_t mask,vint32m2_t merge,int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m2_tum(mask,merge,base,31);
}

vint32m4_t
test___riscv_vle32_v_i32m4_tum(vbool8_t mask,vint32m4_t merge,int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m4_tum(mask,merge,base,31);
}

vint32m8_t
test___riscv_vle32_v_i32m8_tum(vbool4_t mask,vint32m8_t merge,int32_t* base,size_t vl)
{
  return __riscv_vle32_v_i32m8_tum(mask,merge,base,31);
}

vuint32mf2_t
test___riscv_vle32_v_u32mf2_tum(vbool64_t mask,vuint32mf2_t merge,uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32mf2_tum(mask,merge,base,31);
}

vuint32m1_t
test___riscv_vle32_v_u32m1_tum(vbool32_t mask,vuint32m1_t merge,uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m1_tum(mask,merge,base,31);
}

vuint32m2_t
test___riscv_vle32_v_u32m2_tum(vbool16_t mask,vuint32m2_t merge,uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m2_tum(mask,merge,base,31);
}

vuint32m4_t
test___riscv_vle32_v_u32m4_tum(vbool8_t mask,vuint32m4_t merge,uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m4_tum(mask,merge,base,31);
}

vuint32m8_t
test___riscv_vle32_v_u32m8_tum(vbool4_t mask,vuint32m8_t merge,uint32_t* base,size_t vl)
{
  return __riscv_vle32_v_u32m8_tum(mask,merge,base,31);
}

vfloat32mf2_t
test___riscv_vle32_v_f32mf2_tum(vbool64_t mask,vfloat32mf2_t merge,float* base,size_t vl)
{
  return __riscv_vle32_v_f32mf2_tum(mask,merge,base,31);
}

vfloat32m1_t
test___riscv_vle32_v_f32m1_tum(vbool32_t mask,vfloat32m1_t merge,float* base,size_t vl)
{
  return __riscv_vle32_v_f32m1_tum(mask,merge,base,31);
}

vfloat32m2_t
test___riscv_vle32_v_f32m2_tum(vbool16_t mask,vfloat32m2_t merge,float* base,size_t vl)
{
  return __riscv_vle32_v_f32m2_tum(mask,merge,base,31);
}

vfloat32m4_t
test___riscv_vle32_v_f32m4_tum(vbool8_t mask,vfloat32m4_t merge,float* base,size_t vl)
{
  return __riscv_vle32_v_f32m4_tum(mask,merge,base,31);
}

vfloat32m8_t
test___riscv_vle32_v_f32m8_tum(vbool4_t mask,vfloat32m8_t merge,float* base,size_t vl)
{
  return __riscv_vle32_v_f32m8_tum(mask,merge,base,31);
}

vint64m1_t
test___riscv_vle64_v_i64m1_tum(vbool64_t mask,vint64m1_t merge,int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m1_tum(mask,merge,base,31);
}

vint64m2_t
test___riscv_vle64_v_i64m2_tum(vbool32_t mask,vint64m2_t merge,int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m2_tum(mask,merge,base,31);
}

vint64m4_t
test___riscv_vle64_v_i64m4_tum(vbool16_t mask,vint64m4_t merge,int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m4_tum(mask,merge,base,31);
}

vint64m8_t
test___riscv_vle64_v_i64m8_tum(vbool8_t mask,vint64m8_t merge,int64_t* base,size_t vl)
{
  return __riscv_vle64_v_i64m8_tum(mask,merge,base,31);
}

vuint64m1_t
test___riscv_vle64_v_u64m1_tum(vbool64_t mask,vuint64m1_t merge,uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m1_tum(mask,merge,base,31);
}

vuint64m2_t
test___riscv_vle64_v_u64m2_tum(vbool32_t mask,vuint64m2_t merge,uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m2_tum(mask,merge,base,31);
}

vuint64m4_t
test___riscv_vle64_v_u64m4_tum(vbool16_t mask,vuint64m4_t merge,uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m4_tum(mask,merge,base,31);
}

vuint64m8_t
test___riscv_vle64_v_u64m8_tum(vbool8_t mask,vuint64m8_t merge,uint64_t* base,size_t vl)
{
  return __riscv_vle64_v_u64m8_tum(mask,merge,base,31);
}

vfloat64m1_t
test___riscv_vle64_v_f64m1_tum(vbool64_t mask,vfloat64m1_t merge,double* base,size_t vl)
{
  return __riscv_vle64_v_f64m1_tum(mask,merge,base,31);
}

vfloat64m2_t
test___riscv_vle64_v_f64m2_tum(vbool32_t mask,vfloat64m2_t merge,double* base,size_t vl)
{
  return __riscv_vle64_v_f64m2_tum(mask,merge,base,31);
}

vfloat64m4_t
test___riscv_vle64_v_f64m4_tum(vbool16_t mask,vfloat64m4_t merge,double* base,size_t vl)
{
  return __riscv_vle64_v_f64m4_tum(mask,merge,base,31);
}

vfloat64m8_t
test___riscv_vle64_v_f64m8_tum(vbool8_t mask,vfloat64m8_t merge,double* base,size_t vl)
{
  return __riscv_vle64_v_f64m8_tum(mask,merge,base,31);
}

/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*tu,\s*m[au]\s+vle8\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*tu,\s*m[au]\s+vle16\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vle32\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*tu,\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*tu,\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*tu,\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*tu,\s*m[au]\s+vle64\.v\s+v[0-9]+,\s*0\([a-x0-9]+\),\s*v0.t} 3 } } */
