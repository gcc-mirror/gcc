/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vluxei64_v_i8mf8(const int8_t* base,vuint64m1_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i8mf8(base,bindex,vl);
}


vint8mf4_t test___riscv_vluxei64_v_i8mf4(const int8_t* base,vuint64m2_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i8mf4(base,bindex,vl);
}


vint8mf2_t test___riscv_vluxei64_v_i8mf2(const int8_t* base,vuint64m4_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i8mf2(base,bindex,vl);
}


vint8m1_t test___riscv_vluxei64_v_i8m1(const int8_t* base,vuint64m8_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i8m1(base,bindex,vl);
}


vint16mf4_t test___riscv_vluxei64_v_i16mf4(const int16_t* base,vuint64m1_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i16mf4(base,bindex,vl);
}


vint16mf2_t test___riscv_vluxei64_v_i16mf2(const int16_t* base,vuint64m2_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i16mf2(base,bindex,vl);
}


vint16m1_t test___riscv_vluxei64_v_i16m1(const int16_t* base,vuint64m4_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i16m1(base,bindex,vl);
}


vint16m2_t test___riscv_vluxei64_v_i16m2(const int16_t* base,vuint64m8_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i16m2(base,bindex,vl);
}


vint32mf2_t test___riscv_vluxei64_v_i32mf2(const int32_t* base,vuint64m1_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i32mf2(base,bindex,vl);
}


vint32m1_t test___riscv_vluxei64_v_i32m1(const int32_t* base,vuint64m2_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i32m1(base,bindex,vl);
}


vint32m2_t test___riscv_vluxei64_v_i32m2(const int32_t* base,vuint64m4_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i32m2(base,bindex,vl);
}


vint32m4_t test___riscv_vluxei64_v_i32m4(const int32_t* base,vuint64m8_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i32m4(base,bindex,vl);
}


vint64m1_t test___riscv_vluxei64_v_i64m1(const int64_t* base,vuint64m1_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i64m1(base,bindex,vl);
}


vint64m2_t test___riscv_vluxei64_v_i64m2(const int64_t* base,vuint64m2_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i64m2(base,bindex,vl);
}


vint64m4_t test___riscv_vluxei64_v_i64m4(const int64_t* base,vuint64m4_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i64m4(base,bindex,vl);
}


vint64m8_t test___riscv_vluxei64_v_i64m8(const int64_t* base,vuint64m8_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_i64m8(base,bindex,vl);
}


vuint8mf8_t test___riscv_vluxei64_v_u8mf8(const uint8_t* base,vuint64m1_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u8mf8(base,bindex,vl);
}


vuint8mf4_t test___riscv_vluxei64_v_u8mf4(const uint8_t* base,vuint64m2_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u8mf4(base,bindex,vl);
}


vuint8mf2_t test___riscv_vluxei64_v_u8mf2(const uint8_t* base,vuint64m4_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u8mf2(base,bindex,vl);
}


vuint8m1_t test___riscv_vluxei64_v_u8m1(const uint8_t* base,vuint64m8_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u8m1(base,bindex,vl);
}


vuint16mf4_t test___riscv_vluxei64_v_u16mf4(const uint16_t* base,vuint64m1_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u16mf4(base,bindex,vl);
}


vuint16mf2_t test___riscv_vluxei64_v_u16mf2(const uint16_t* base,vuint64m2_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u16mf2(base,bindex,vl);
}


vuint16m1_t test___riscv_vluxei64_v_u16m1(const uint16_t* base,vuint64m4_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u16m1(base,bindex,vl);
}


vuint16m2_t test___riscv_vluxei64_v_u16m2(const uint16_t* base,vuint64m8_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u16m2(base,bindex,vl);
}


vuint32mf2_t test___riscv_vluxei64_v_u32mf2(const uint32_t* base,vuint64m1_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u32mf2(base,bindex,vl);
}


vuint32m1_t test___riscv_vluxei64_v_u32m1(const uint32_t* base,vuint64m2_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u32m1(base,bindex,vl);
}


vuint32m2_t test___riscv_vluxei64_v_u32m2(const uint32_t* base,vuint64m4_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u32m2(base,bindex,vl);
}


vuint32m4_t test___riscv_vluxei64_v_u32m4(const uint32_t* base,vuint64m8_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u32m4(base,bindex,vl);
}


vuint64m1_t test___riscv_vluxei64_v_u64m1(const uint64_t* base,vuint64m1_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u64m1(base,bindex,vl);
}


vuint64m2_t test___riscv_vluxei64_v_u64m2(const uint64_t* base,vuint64m2_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u64m2(base,bindex,vl);
}


vuint64m4_t test___riscv_vluxei64_v_u64m4(const uint64_t* base,vuint64m4_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u64m4(base,bindex,vl);
}


vuint64m8_t test___riscv_vluxei64_v_u64m8(const uint64_t* base,vuint64m8_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_u64m8(base,bindex,vl);
}


vfloat32mf2_t test___riscv_vluxei64_v_f32mf2(const float* base,vuint64m1_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_f32mf2(base,bindex,vl);
}


vfloat32m1_t test___riscv_vluxei64_v_f32m1(const float* base,vuint64m2_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_f32m1(base,bindex,vl);
}


vfloat32m2_t test___riscv_vluxei64_v_f32m2(const float* base,vuint64m4_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_f32m2(base,bindex,vl);
}


vfloat32m4_t test___riscv_vluxei64_v_f32m4(const float* base,vuint64m8_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_f32m4(base,bindex,vl);
}


vfloat64m1_t test___riscv_vluxei64_v_f64m1(const double* base,vuint64m1_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_f64m1(base,bindex,vl);
}


vfloat64m2_t test___riscv_vluxei64_v_f64m2(const double* base,vuint64m2_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_f64m2(base,bindex,vl);
}


vfloat64m4_t test___riscv_vluxei64_v_f64m4(const double* base,vuint64m4_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_f64m4(base,bindex,vl);
}


vfloat64m8_t test___riscv_vluxei64_v_f64m8(const double* base,vuint64m8_t bindex,size_t vl)
{
    return __riscv_vluxei64_v_f64m8(base,bindex,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vluxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
