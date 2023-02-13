/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vloxei8_tum(vbool64_t mask,vint8mf8_t merge,const int8_t* base,vuint8mf8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint8mf4_t test___riscv_vloxei8_tum(vbool32_t mask,vint8mf4_t merge,const int8_t* base,vuint8mf4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint8mf2_t test___riscv_vloxei8_tum(vbool16_t mask,vint8mf2_t merge,const int8_t* base,vuint8mf2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint8m1_t test___riscv_vloxei8_tum(vbool8_t mask,vint8m1_t merge,const int8_t* base,vuint8m1_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint8m2_t test___riscv_vloxei8_tum(vbool4_t mask,vint8m2_t merge,const int8_t* base,vuint8m2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint8m4_t test___riscv_vloxei8_tum(vbool2_t mask,vint8m4_t merge,const int8_t* base,vuint8m4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint8m8_t test___riscv_vloxei8_tum(vbool1_t mask,vint8m8_t merge,const int8_t* base,vuint8m8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint16mf4_t test___riscv_vloxei8_tum(vbool64_t mask,vint16mf4_t merge,const int16_t* base,vuint8mf8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint16mf2_t test___riscv_vloxei8_tum(vbool32_t mask,vint16mf2_t merge,const int16_t* base,vuint8mf4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint16m1_t test___riscv_vloxei8_tum(vbool16_t mask,vint16m1_t merge,const int16_t* base,vuint8mf2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint16m2_t test___riscv_vloxei8_tum(vbool8_t mask,vint16m2_t merge,const int16_t* base,vuint8m1_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint16m4_t test___riscv_vloxei8_tum(vbool4_t mask,vint16m4_t merge,const int16_t* base,vuint8m2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint16m8_t test___riscv_vloxei8_tum(vbool2_t mask,vint16m8_t merge,const int16_t* base,vuint8m4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint32mf2_t test___riscv_vloxei8_tum(vbool64_t mask,vint32mf2_t merge,const int32_t* base,vuint8mf8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint32m1_t test___riscv_vloxei8_tum(vbool32_t mask,vint32m1_t merge,const int32_t* base,vuint8mf4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint32m2_t test___riscv_vloxei8_tum(vbool16_t mask,vint32m2_t merge,const int32_t* base,vuint8mf2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint32m4_t test___riscv_vloxei8_tum(vbool8_t mask,vint32m4_t merge,const int32_t* base,vuint8m1_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint32m8_t test___riscv_vloxei8_tum(vbool4_t mask,vint32m8_t merge,const int32_t* base,vuint8m2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint64m1_t test___riscv_vloxei8_tum(vbool64_t mask,vint64m1_t merge,const int64_t* base,vuint8mf8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint64m2_t test___riscv_vloxei8_tum(vbool32_t mask,vint64m2_t merge,const int64_t* base,vuint8mf4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint64m4_t test___riscv_vloxei8_tum(vbool16_t mask,vint64m4_t merge,const int64_t* base,vuint8mf2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vint64m8_t test___riscv_vloxei8_tum(vbool8_t mask,vint64m8_t merge,const int64_t* base,vuint8m1_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint8mf8_t test___riscv_vloxei8_tum(vbool64_t mask,vuint8mf8_t merge,const uint8_t* base,vuint8mf8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint8mf4_t test___riscv_vloxei8_tum(vbool32_t mask,vuint8mf4_t merge,const uint8_t* base,vuint8mf4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint8mf2_t test___riscv_vloxei8_tum(vbool16_t mask,vuint8mf2_t merge,const uint8_t* base,vuint8mf2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint8m1_t test___riscv_vloxei8_tum(vbool8_t mask,vuint8m1_t merge,const uint8_t* base,vuint8m1_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint8m2_t test___riscv_vloxei8_tum(vbool4_t mask,vuint8m2_t merge,const uint8_t* base,vuint8m2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint8m4_t test___riscv_vloxei8_tum(vbool2_t mask,vuint8m4_t merge,const uint8_t* base,vuint8m4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint8m8_t test___riscv_vloxei8_tum(vbool1_t mask,vuint8m8_t merge,const uint8_t* base,vuint8m8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint16mf4_t test___riscv_vloxei8_tum(vbool64_t mask,vuint16mf4_t merge,const uint16_t* base,vuint8mf8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint16mf2_t test___riscv_vloxei8_tum(vbool32_t mask,vuint16mf2_t merge,const uint16_t* base,vuint8mf4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint16m1_t test___riscv_vloxei8_tum(vbool16_t mask,vuint16m1_t merge,const uint16_t* base,vuint8mf2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint16m2_t test___riscv_vloxei8_tum(vbool8_t mask,vuint16m2_t merge,const uint16_t* base,vuint8m1_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint16m4_t test___riscv_vloxei8_tum(vbool4_t mask,vuint16m4_t merge,const uint16_t* base,vuint8m2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint16m8_t test___riscv_vloxei8_tum(vbool2_t mask,vuint16m8_t merge,const uint16_t* base,vuint8m4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint32mf2_t test___riscv_vloxei8_tum(vbool64_t mask,vuint32mf2_t merge,const uint32_t* base,vuint8mf8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint32m1_t test___riscv_vloxei8_tum(vbool32_t mask,vuint32m1_t merge,const uint32_t* base,vuint8mf4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint32m2_t test___riscv_vloxei8_tum(vbool16_t mask,vuint32m2_t merge,const uint32_t* base,vuint8mf2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint32m4_t test___riscv_vloxei8_tum(vbool8_t mask,vuint32m4_t merge,const uint32_t* base,vuint8m1_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint32m8_t test___riscv_vloxei8_tum(vbool4_t mask,vuint32m8_t merge,const uint32_t* base,vuint8m2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint64m1_t test___riscv_vloxei8_tum(vbool64_t mask,vuint64m1_t merge,const uint64_t* base,vuint8mf8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint64m2_t test___riscv_vloxei8_tum(vbool32_t mask,vuint64m2_t merge,const uint64_t* base,vuint8mf4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint64m4_t test___riscv_vloxei8_tum(vbool16_t mask,vuint64m4_t merge,const uint64_t* base,vuint8mf2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vuint64m8_t test___riscv_vloxei8_tum(vbool8_t mask,vuint64m8_t merge,const uint64_t* base,vuint8m1_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vfloat32mf2_t test___riscv_vloxei8_tum(vbool64_t mask,vfloat32mf2_t merge,const float* base,vuint8mf8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vfloat32m1_t test___riscv_vloxei8_tum(vbool32_t mask,vfloat32m1_t merge,const float* base,vuint8mf4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vfloat32m2_t test___riscv_vloxei8_tum(vbool16_t mask,vfloat32m2_t merge,const float* base,vuint8mf2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vfloat32m4_t test___riscv_vloxei8_tum(vbool8_t mask,vfloat32m4_t merge,const float* base,vuint8m1_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vfloat32m8_t test___riscv_vloxei8_tum(vbool4_t mask,vfloat32m8_t merge,const float* base,vuint8m2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vfloat64m1_t test___riscv_vloxei8_tum(vbool64_t mask,vfloat64m1_t merge,const double* base,vuint8mf8_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vfloat64m2_t test___riscv_vloxei8_tum(vbool32_t mask,vfloat64m2_t merge,const double* base,vuint8mf4_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vfloat64m4_t test___riscv_vloxei8_tum(vbool16_t mask,vfloat64m4_t merge,const double* base,vuint8mf2_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}


vfloat64m8_t test___riscv_vloxei8_tum(vbool8_t mask,vfloat64m8_t merge,const double* base,vuint8m1_t bindex,size_t vl)
{
    return __riscv_vloxei8_tum(mask,merge,base,bindex,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*tu,\s*m[au]\s+vloxei8\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
