/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vluxei32_v_i8mf8_m(vbool64_t mask,const int8_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i8mf8_m(mask,base,bindex,31);
}


vint8mf4_t test___riscv_vluxei32_v_i8mf4_m(vbool32_t mask,const int8_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i8mf4_m(mask,base,bindex,31);
}


vint8mf2_t test___riscv_vluxei32_v_i8mf2_m(vbool16_t mask,const int8_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i8mf2_m(mask,base,bindex,31);
}


vint8m1_t test___riscv_vluxei32_v_i8m1_m(vbool8_t mask,const int8_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i8m1_m(mask,base,bindex,31);
}


vint8m2_t test___riscv_vluxei32_v_i8m2_m(vbool4_t mask,const int8_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i8m2_m(mask,base,bindex,31);
}


vint16mf4_t test___riscv_vluxei32_v_i16mf4_m(vbool64_t mask,const int16_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i16mf4_m(mask,base,bindex,31);
}


vint16mf2_t test___riscv_vluxei32_v_i16mf2_m(vbool32_t mask,const int16_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i16mf2_m(mask,base,bindex,31);
}


vint16m1_t test___riscv_vluxei32_v_i16m1_m(vbool16_t mask,const int16_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i16m1_m(mask,base,bindex,31);
}


vint16m2_t test___riscv_vluxei32_v_i16m2_m(vbool8_t mask,const int16_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i16m2_m(mask,base,bindex,31);
}


vint16m4_t test___riscv_vluxei32_v_i16m4_m(vbool4_t mask,const int16_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i16m4_m(mask,base,bindex,31);
}


vint32mf2_t test___riscv_vluxei32_v_i32mf2_m(vbool64_t mask,const int32_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i32mf2_m(mask,base,bindex,31);
}


vint32m1_t test___riscv_vluxei32_v_i32m1_m(vbool32_t mask,const int32_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i32m1_m(mask,base,bindex,31);
}


vint32m2_t test___riscv_vluxei32_v_i32m2_m(vbool16_t mask,const int32_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i32m2_m(mask,base,bindex,31);
}


vint32m4_t test___riscv_vluxei32_v_i32m4_m(vbool8_t mask,const int32_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i32m4_m(mask,base,bindex,31);
}


vint32m8_t test___riscv_vluxei32_v_i32m8_m(vbool4_t mask,const int32_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i32m8_m(mask,base,bindex,31);
}


vint64m1_t test___riscv_vluxei32_v_i64m1_m(vbool64_t mask,const int64_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i64m1_m(mask,base,bindex,31);
}


vint64m2_t test___riscv_vluxei32_v_i64m2_m(vbool32_t mask,const int64_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i64m2_m(mask,base,bindex,31);
}


vint64m4_t test___riscv_vluxei32_v_i64m4_m(vbool16_t mask,const int64_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i64m4_m(mask,base,bindex,31);
}


vint64m8_t test___riscv_vluxei32_v_i64m8_m(vbool8_t mask,const int64_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_i64m8_m(mask,base,bindex,31);
}


vuint8mf8_t test___riscv_vluxei32_v_u8mf8_m(vbool64_t mask,const uint8_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u8mf8_m(mask,base,bindex,31);
}


vuint8mf4_t test___riscv_vluxei32_v_u8mf4_m(vbool32_t mask,const uint8_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u8mf4_m(mask,base,bindex,31);
}


vuint8mf2_t test___riscv_vluxei32_v_u8mf2_m(vbool16_t mask,const uint8_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u8mf2_m(mask,base,bindex,31);
}


vuint8m1_t test___riscv_vluxei32_v_u8m1_m(vbool8_t mask,const uint8_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u8m1_m(mask,base,bindex,31);
}


vuint8m2_t test___riscv_vluxei32_v_u8m2_m(vbool4_t mask,const uint8_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u8m2_m(mask,base,bindex,31);
}


vuint16mf4_t test___riscv_vluxei32_v_u16mf4_m(vbool64_t mask,const uint16_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u16mf4_m(mask,base,bindex,31);
}


vuint16mf2_t test___riscv_vluxei32_v_u16mf2_m(vbool32_t mask,const uint16_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u16mf2_m(mask,base,bindex,31);
}


vuint16m1_t test___riscv_vluxei32_v_u16m1_m(vbool16_t mask,const uint16_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u16m1_m(mask,base,bindex,31);
}


vuint16m2_t test___riscv_vluxei32_v_u16m2_m(vbool8_t mask,const uint16_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u16m2_m(mask,base,bindex,31);
}


vuint16m4_t test___riscv_vluxei32_v_u16m4_m(vbool4_t mask,const uint16_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u16m4_m(mask,base,bindex,31);
}


vuint32mf2_t test___riscv_vluxei32_v_u32mf2_m(vbool64_t mask,const uint32_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u32mf2_m(mask,base,bindex,31);
}


vuint32m1_t test___riscv_vluxei32_v_u32m1_m(vbool32_t mask,const uint32_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u32m1_m(mask,base,bindex,31);
}


vuint32m2_t test___riscv_vluxei32_v_u32m2_m(vbool16_t mask,const uint32_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u32m2_m(mask,base,bindex,31);
}


vuint32m4_t test___riscv_vluxei32_v_u32m4_m(vbool8_t mask,const uint32_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u32m4_m(mask,base,bindex,31);
}


vuint32m8_t test___riscv_vluxei32_v_u32m8_m(vbool4_t mask,const uint32_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u32m8_m(mask,base,bindex,31);
}


vuint64m1_t test___riscv_vluxei32_v_u64m1_m(vbool64_t mask,const uint64_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u64m1_m(mask,base,bindex,31);
}


vuint64m2_t test___riscv_vluxei32_v_u64m2_m(vbool32_t mask,const uint64_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u64m2_m(mask,base,bindex,31);
}


vuint64m4_t test___riscv_vluxei32_v_u64m4_m(vbool16_t mask,const uint64_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u64m4_m(mask,base,bindex,31);
}


vuint64m8_t test___riscv_vluxei32_v_u64m8_m(vbool8_t mask,const uint64_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_u64m8_m(mask,base,bindex,31);
}


vfloat32mf2_t test___riscv_vluxei32_v_f32mf2_m(vbool64_t mask,const float* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_f32mf2_m(mask,base,bindex,31);
}


vfloat32m1_t test___riscv_vluxei32_v_f32m1_m(vbool32_t mask,const float* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_f32m1_m(mask,base,bindex,31);
}


vfloat32m2_t test___riscv_vluxei32_v_f32m2_m(vbool16_t mask,const float* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_f32m2_m(mask,base,bindex,31);
}


vfloat32m4_t test___riscv_vluxei32_v_f32m4_m(vbool8_t mask,const float* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_f32m4_m(mask,base,bindex,31);
}


vfloat32m8_t test___riscv_vluxei32_v_f32m8_m(vbool4_t mask,const float* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_f32m8_m(mask,base,bindex,31);
}


vfloat64m1_t test___riscv_vluxei32_v_f64m1_m(vbool64_t mask,const double* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_f64m1_m(mask,base,bindex,31);
}


vfloat64m2_t test___riscv_vluxei32_v_f64m2_m(vbool32_t mask,const double* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_f64m2_m(mask,base,bindex,31);
}


vfloat64m4_t test___riscv_vluxei32_v_f64m4_m(vbool16_t mask,const double* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_f64m4_m(mask,base,bindex,31);
}


vfloat64m8_t test___riscv_vluxei32_v_f64m8_m(vbool8_t mask,const double* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32_v_f64m8_m(mask,base,bindex,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
