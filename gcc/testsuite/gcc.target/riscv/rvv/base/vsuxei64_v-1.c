/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void test___riscv_vsuxei64_v_i8mf8(int8_t* base,vuint64m1_t bindex,vint8mf8_t value,size_t vl)
{
    __riscv_vsuxei64_v_i8mf8(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i8mf4(int8_t* base,vuint64m2_t bindex,vint8mf4_t value,size_t vl)
{
    __riscv_vsuxei64_v_i8mf4(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i8mf2(int8_t* base,vuint64m4_t bindex,vint8mf2_t value,size_t vl)
{
    __riscv_vsuxei64_v_i8mf2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i8m1(int8_t* base,vuint64m8_t bindex,vint8m1_t value,size_t vl)
{
    __riscv_vsuxei64_v_i8m1(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i16mf4(int16_t* base,vuint64m1_t bindex,vint16mf4_t value,size_t vl)
{
    __riscv_vsuxei64_v_i16mf4(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i16mf2(int16_t* base,vuint64m2_t bindex,vint16mf2_t value,size_t vl)
{
    __riscv_vsuxei64_v_i16mf2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i16m1(int16_t* base,vuint64m4_t bindex,vint16m1_t value,size_t vl)
{
    __riscv_vsuxei64_v_i16m1(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i16m2(int16_t* base,vuint64m8_t bindex,vint16m2_t value,size_t vl)
{
    __riscv_vsuxei64_v_i16m2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i32mf2(int32_t* base,vuint64m1_t bindex,vint32mf2_t value,size_t vl)
{
    __riscv_vsuxei64_v_i32mf2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i32m1(int32_t* base,vuint64m2_t bindex,vint32m1_t value,size_t vl)
{
    __riscv_vsuxei64_v_i32m1(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i32m2(int32_t* base,vuint64m4_t bindex,vint32m2_t value,size_t vl)
{
    __riscv_vsuxei64_v_i32m2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i32m4(int32_t* base,vuint64m8_t bindex,vint32m4_t value,size_t vl)
{
    __riscv_vsuxei64_v_i32m4(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i64m1(int64_t* base,vuint64m1_t bindex,vint64m1_t value,size_t vl)
{
    __riscv_vsuxei64_v_i64m1(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i64m2(int64_t* base,vuint64m2_t bindex,vint64m2_t value,size_t vl)
{
    __riscv_vsuxei64_v_i64m2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i64m4(int64_t* base,vuint64m4_t bindex,vint64m4_t value,size_t vl)
{
    __riscv_vsuxei64_v_i64m4(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_i64m8(int64_t* base,vuint64m8_t bindex,vint64m8_t value,size_t vl)
{
    __riscv_vsuxei64_v_i64m8(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u8mf8(uint8_t* base,vuint64m1_t bindex,vuint8mf8_t value,size_t vl)
{
    __riscv_vsuxei64_v_u8mf8(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u8mf4(uint8_t* base,vuint64m2_t bindex,vuint8mf4_t value,size_t vl)
{
    __riscv_vsuxei64_v_u8mf4(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u8mf2(uint8_t* base,vuint64m4_t bindex,vuint8mf2_t value,size_t vl)
{
    __riscv_vsuxei64_v_u8mf2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u8m1(uint8_t* base,vuint64m8_t bindex,vuint8m1_t value,size_t vl)
{
    __riscv_vsuxei64_v_u8m1(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u16mf4(uint16_t* base,vuint64m1_t bindex,vuint16mf4_t value,size_t vl)
{
    __riscv_vsuxei64_v_u16mf4(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u16mf2(uint16_t* base,vuint64m2_t bindex,vuint16mf2_t value,size_t vl)
{
    __riscv_vsuxei64_v_u16mf2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u16m1(uint16_t* base,vuint64m4_t bindex,vuint16m1_t value,size_t vl)
{
    __riscv_vsuxei64_v_u16m1(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u16m2(uint16_t* base,vuint64m8_t bindex,vuint16m2_t value,size_t vl)
{
    __riscv_vsuxei64_v_u16m2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u32mf2(uint32_t* base,vuint64m1_t bindex,vuint32mf2_t value,size_t vl)
{
    __riscv_vsuxei64_v_u32mf2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u32m1(uint32_t* base,vuint64m2_t bindex,vuint32m1_t value,size_t vl)
{
    __riscv_vsuxei64_v_u32m1(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u32m2(uint32_t* base,vuint64m4_t bindex,vuint32m2_t value,size_t vl)
{
    __riscv_vsuxei64_v_u32m2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u32m4(uint32_t* base,vuint64m8_t bindex,vuint32m4_t value,size_t vl)
{
    __riscv_vsuxei64_v_u32m4(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u64m1(uint64_t* base,vuint64m1_t bindex,vuint64m1_t value,size_t vl)
{
    __riscv_vsuxei64_v_u64m1(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u64m2(uint64_t* base,vuint64m2_t bindex,vuint64m2_t value,size_t vl)
{
    __riscv_vsuxei64_v_u64m2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u64m4(uint64_t* base,vuint64m4_t bindex,vuint64m4_t value,size_t vl)
{
    __riscv_vsuxei64_v_u64m4(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_u64m8(uint64_t* base,vuint64m8_t bindex,vuint64m8_t value,size_t vl)
{
    __riscv_vsuxei64_v_u64m8(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_f32mf2(float* base,vuint64m1_t bindex,vfloat32mf2_t value,size_t vl)
{
    __riscv_vsuxei64_v_f32mf2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_f32m1(float* base,vuint64m2_t bindex,vfloat32m1_t value,size_t vl)
{
    __riscv_vsuxei64_v_f32m1(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_f32m2(float* base,vuint64m4_t bindex,vfloat32m2_t value,size_t vl)
{
    __riscv_vsuxei64_v_f32m2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_f32m4(float* base,vuint64m8_t bindex,vfloat32m4_t value,size_t vl)
{
    __riscv_vsuxei64_v_f32m4(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_f64m1(double* base,vuint64m1_t bindex,vfloat64m1_t value,size_t vl)
{
    __riscv_vsuxei64_v_f64m1(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_f64m2(double* base,vuint64m2_t bindex,vfloat64m2_t value,size_t vl)
{
    __riscv_vsuxei64_v_f64m2(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_f64m4(double* base,vuint64m4_t bindex,vfloat64m4_t value,size_t vl)
{
    __riscv_vsuxei64_v_f64m4(base,bindex,value,vl);
}


void test___riscv_vsuxei64_v_f64m8(double* base,vuint64m8_t bindex,vfloat64m8_t value,size_t vl)
{
    __riscv_vsuxei64_v_f64m8(base,bindex,value,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsuxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+} 3 } } */
