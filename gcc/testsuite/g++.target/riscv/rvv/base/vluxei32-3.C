/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vluxei32(const int8_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint8mf4_t test___riscv_vluxei32(const int8_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint8mf2_t test___riscv_vluxei32(const int8_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint8m1_t test___riscv_vluxei32(const int8_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint8m2_t test___riscv_vluxei32(const int8_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint16mf4_t test___riscv_vluxei32(const int16_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint16mf2_t test___riscv_vluxei32(const int16_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint16m1_t test___riscv_vluxei32(const int16_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint16m2_t test___riscv_vluxei32(const int16_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint16m4_t test___riscv_vluxei32(const int16_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint32mf2_t test___riscv_vluxei32(const int32_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint32m1_t test___riscv_vluxei32(const int32_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint32m2_t test___riscv_vluxei32(const int32_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint32m4_t test___riscv_vluxei32(const int32_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint32m8_t test___riscv_vluxei32(const int32_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint64m1_t test___riscv_vluxei32(const int64_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint64m2_t test___riscv_vluxei32(const int64_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint64m4_t test___riscv_vluxei32(const int64_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint64m8_t test___riscv_vluxei32(const int64_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint8mf8_t test___riscv_vluxei32(const uint8_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint8mf4_t test___riscv_vluxei32(const uint8_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint8mf2_t test___riscv_vluxei32(const uint8_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint8m1_t test___riscv_vluxei32(const uint8_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint8m2_t test___riscv_vluxei32(const uint8_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint16mf4_t test___riscv_vluxei32(const uint16_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint16mf2_t test___riscv_vluxei32(const uint16_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint16m1_t test___riscv_vluxei32(const uint16_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint16m2_t test___riscv_vluxei32(const uint16_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint16m4_t test___riscv_vluxei32(const uint16_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint32mf2_t test___riscv_vluxei32(const uint32_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint32m1_t test___riscv_vluxei32(const uint32_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint32m2_t test___riscv_vluxei32(const uint32_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint32m4_t test___riscv_vluxei32(const uint32_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint32m8_t test___riscv_vluxei32(const uint32_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint64m1_t test___riscv_vluxei32(const uint64_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint64m2_t test___riscv_vluxei32(const uint64_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint64m4_t test___riscv_vluxei32(const uint64_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vuint64m8_t test___riscv_vluxei32(const uint64_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vfloat32mf2_t test___riscv_vluxei32(const float* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vfloat32m1_t test___riscv_vluxei32(const float* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vfloat32m2_t test___riscv_vluxei32(const float* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vfloat32m4_t test___riscv_vluxei32(const float* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vfloat32m8_t test___riscv_vluxei32(const float* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vfloat64m1_t test___riscv_vluxei32(const double* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vfloat64m2_t test___riscv_vluxei32(const double* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vfloat64m4_t test___riscv_vluxei32(const double* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vfloat64m8_t test___riscv_vluxei32(const double* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(base,bindex,32);
}


vint8mf8_t test___riscv_vluxei32(vbool64_t mask,const int8_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint8mf4_t test___riscv_vluxei32(vbool32_t mask,const int8_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint8mf2_t test___riscv_vluxei32(vbool16_t mask,const int8_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint8m1_t test___riscv_vluxei32(vbool8_t mask,const int8_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint8m2_t test___riscv_vluxei32(vbool4_t mask,const int8_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint16mf4_t test___riscv_vluxei32(vbool64_t mask,const int16_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint16mf2_t test___riscv_vluxei32(vbool32_t mask,const int16_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint16m1_t test___riscv_vluxei32(vbool16_t mask,const int16_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint16m2_t test___riscv_vluxei32(vbool8_t mask,const int16_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint16m4_t test___riscv_vluxei32(vbool4_t mask,const int16_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint32mf2_t test___riscv_vluxei32(vbool64_t mask,const int32_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint32m1_t test___riscv_vluxei32(vbool32_t mask,const int32_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint32m2_t test___riscv_vluxei32(vbool16_t mask,const int32_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint32m4_t test___riscv_vluxei32(vbool8_t mask,const int32_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint32m8_t test___riscv_vluxei32(vbool4_t mask,const int32_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint64m1_t test___riscv_vluxei32(vbool64_t mask,const int64_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint64m2_t test___riscv_vluxei32(vbool32_t mask,const int64_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint64m4_t test___riscv_vluxei32(vbool16_t mask,const int64_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vint64m8_t test___riscv_vluxei32(vbool8_t mask,const int64_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint8mf8_t test___riscv_vluxei32(vbool64_t mask,const uint8_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint8mf4_t test___riscv_vluxei32(vbool32_t mask,const uint8_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint8mf2_t test___riscv_vluxei32(vbool16_t mask,const uint8_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint8m1_t test___riscv_vluxei32(vbool8_t mask,const uint8_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint8m2_t test___riscv_vluxei32(vbool4_t mask,const uint8_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint16mf4_t test___riscv_vluxei32(vbool64_t mask,const uint16_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint16mf2_t test___riscv_vluxei32(vbool32_t mask,const uint16_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint16m1_t test___riscv_vluxei32(vbool16_t mask,const uint16_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint16m2_t test___riscv_vluxei32(vbool8_t mask,const uint16_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint16m4_t test___riscv_vluxei32(vbool4_t mask,const uint16_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint32mf2_t test___riscv_vluxei32(vbool64_t mask,const uint32_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint32m1_t test___riscv_vluxei32(vbool32_t mask,const uint32_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint32m2_t test___riscv_vluxei32(vbool16_t mask,const uint32_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint32m4_t test___riscv_vluxei32(vbool8_t mask,const uint32_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint32m8_t test___riscv_vluxei32(vbool4_t mask,const uint32_t* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint64m1_t test___riscv_vluxei32(vbool64_t mask,const uint64_t* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint64m2_t test___riscv_vluxei32(vbool32_t mask,const uint64_t* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint64m4_t test___riscv_vluxei32(vbool16_t mask,const uint64_t* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vuint64m8_t test___riscv_vluxei32(vbool8_t mask,const uint64_t* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vfloat32mf2_t test___riscv_vluxei32(vbool64_t mask,const float* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vfloat32m1_t test___riscv_vluxei32(vbool32_t mask,const float* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vfloat32m2_t test___riscv_vluxei32(vbool16_t mask,const float* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vfloat32m4_t test___riscv_vluxei32(vbool8_t mask,const float* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vfloat32m8_t test___riscv_vluxei32(vbool4_t mask,const float* base,vuint32m8_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vfloat64m1_t test___riscv_vluxei32(vbool64_t mask,const double* base,vuint32mf2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vfloat64m2_t test___riscv_vluxei32(vbool32_t mask,const double* base,vuint32m1_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vfloat64m4_t test___riscv_vluxei32(vbool16_t mask,const double* base,vuint32m2_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}


vfloat64m8_t test___riscv_vluxei32(vbool8_t mask,const double* base,vuint32m4_t bindex,size_t vl)
{
    return __riscv_vluxei32(mask,base,bindex,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vluxei32\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
