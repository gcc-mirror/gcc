/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

void test___riscv_vsoxei64(int8_t* base,vuint64m1_t bindex,vint8mf8_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int8_t* base,vuint64m2_t bindex,vint8mf4_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int8_t* base,vuint64m4_t bindex,vint8mf2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int8_t* base,vuint64m8_t bindex,vint8m1_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int16_t* base,vuint64m1_t bindex,vint16mf4_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int16_t* base,vuint64m2_t bindex,vint16mf2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int16_t* base,vuint64m4_t bindex,vint16m1_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int16_t* base,vuint64m8_t bindex,vint16m2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int32_t* base,vuint64m1_t bindex,vint32mf2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int32_t* base,vuint64m2_t bindex,vint32m1_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int32_t* base,vuint64m4_t bindex,vint32m2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int32_t* base,vuint64m8_t bindex,vint32m4_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int64_t* base,vuint64m1_t bindex,vint64m1_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int64_t* base,vuint64m2_t bindex,vint64m2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int64_t* base,vuint64m4_t bindex,vint64m4_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(int64_t* base,vuint64m8_t bindex,vint64m8_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint8_t* base,vuint64m1_t bindex,vuint8mf8_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint8_t* base,vuint64m2_t bindex,vuint8mf4_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint8_t* base,vuint64m4_t bindex,vuint8mf2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint8_t* base,vuint64m8_t bindex,vuint8m1_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint16_t* base,vuint64m1_t bindex,vuint16mf4_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint16_t* base,vuint64m2_t bindex,vuint16mf2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint16_t* base,vuint64m4_t bindex,vuint16m1_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint16_t* base,vuint64m8_t bindex,vuint16m2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint32_t* base,vuint64m1_t bindex,vuint32mf2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint32_t* base,vuint64m2_t bindex,vuint32m1_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint32_t* base,vuint64m4_t bindex,vuint32m2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint32_t* base,vuint64m8_t bindex,vuint32m4_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint64_t* base,vuint64m1_t bindex,vuint64m1_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint64_t* base,vuint64m2_t bindex,vuint64m2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint64_t* base,vuint64m4_t bindex,vuint64m4_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(uint64_t* base,vuint64m8_t bindex,vuint64m8_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(float* base,vuint64m1_t bindex,vfloat32mf2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(float* base,vuint64m2_t bindex,vfloat32m1_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(float* base,vuint64m4_t bindex,vfloat32m2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(float* base,vuint64m8_t bindex,vfloat32m4_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(double* base,vuint64m1_t bindex,vfloat64m1_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(double* base,vuint64m2_t bindex,vfloat64m2_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(double* base,vuint64m4_t bindex,vfloat64m4_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(double* base,vuint64m8_t bindex,vfloat64m8_t value,size_t vl)
{
    __riscv_vsoxei64(base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool64_t mask,int8_t* base,vuint64m1_t bindex,vint8mf8_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool32_t mask,int8_t* base,vuint64m2_t bindex,vint8mf4_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool16_t mask,int8_t* base,vuint64m4_t bindex,vint8mf2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool8_t mask,int8_t* base,vuint64m8_t bindex,vint8m1_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool64_t mask,int16_t* base,vuint64m1_t bindex,vint16mf4_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool32_t mask,int16_t* base,vuint64m2_t bindex,vint16mf2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool16_t mask,int16_t* base,vuint64m4_t bindex,vint16m1_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool8_t mask,int16_t* base,vuint64m8_t bindex,vint16m2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool64_t mask,int32_t* base,vuint64m1_t bindex,vint32mf2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool32_t mask,int32_t* base,vuint64m2_t bindex,vint32m1_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool16_t mask,int32_t* base,vuint64m4_t bindex,vint32m2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool8_t mask,int32_t* base,vuint64m8_t bindex,vint32m4_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool64_t mask,int64_t* base,vuint64m1_t bindex,vint64m1_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool32_t mask,int64_t* base,vuint64m2_t bindex,vint64m2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool16_t mask,int64_t* base,vuint64m4_t bindex,vint64m4_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool8_t mask,int64_t* base,vuint64m8_t bindex,vint64m8_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool64_t mask,uint8_t* base,vuint64m1_t bindex,vuint8mf8_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool32_t mask,uint8_t* base,vuint64m2_t bindex,vuint8mf4_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool16_t mask,uint8_t* base,vuint64m4_t bindex,vuint8mf2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool8_t mask,uint8_t* base,vuint64m8_t bindex,vuint8m1_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool64_t mask,uint16_t* base,vuint64m1_t bindex,vuint16mf4_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool32_t mask,uint16_t* base,vuint64m2_t bindex,vuint16mf2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool16_t mask,uint16_t* base,vuint64m4_t bindex,vuint16m1_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool8_t mask,uint16_t* base,vuint64m8_t bindex,vuint16m2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool64_t mask,uint32_t* base,vuint64m1_t bindex,vuint32mf2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool32_t mask,uint32_t* base,vuint64m2_t bindex,vuint32m1_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool16_t mask,uint32_t* base,vuint64m4_t bindex,vuint32m2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool8_t mask,uint32_t* base,vuint64m8_t bindex,vuint32m4_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool64_t mask,uint64_t* base,vuint64m1_t bindex,vuint64m1_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool32_t mask,uint64_t* base,vuint64m2_t bindex,vuint64m2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool16_t mask,uint64_t* base,vuint64m4_t bindex,vuint64m4_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool8_t mask,uint64_t* base,vuint64m8_t bindex,vuint64m8_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool64_t mask,float* base,vuint64m1_t bindex,vfloat32mf2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool32_t mask,float* base,vuint64m2_t bindex,vfloat32m1_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool16_t mask,float* base,vuint64m4_t bindex,vfloat32m2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool8_t mask,float* base,vuint64m8_t bindex,vfloat32m4_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool64_t mask,double* base,vuint64m1_t bindex,vfloat64m1_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool32_t mask,double* base,vuint64m2_t bindex,vfloat64m2_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool16_t mask,double* base,vuint64m4_t bindex,vfloat64m4_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}


void test___riscv_vsoxei64(vbool8_t mask,double* base,vuint64m8_t bindex,vfloat64m8_t value,size_t vl)
{
    __riscv_vsoxei64(mask,base,bindex,value,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+\s+} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsoxei64\.v\s+v[0-9]+,\s*0?\([a-x0-9]+\),\s*v[0-9]+,\s*v0.t} 3 } } */
