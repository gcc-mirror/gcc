/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vmulhsu_vx_i8mf8_m(vbool64_t mask,vint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i8mf8_m(mask,op1,op2,32);
}


vint8mf4_t test___riscv_vmulhsu_vx_i8mf4_m(vbool32_t mask,vint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i8mf4_m(mask,op1,op2,32);
}


vint8mf2_t test___riscv_vmulhsu_vx_i8mf2_m(vbool16_t mask,vint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i8mf2_m(mask,op1,op2,32);
}


vint8m1_t test___riscv_vmulhsu_vx_i8m1_m(vbool8_t mask,vint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i8m1_m(mask,op1,op2,32);
}


vint8m2_t test___riscv_vmulhsu_vx_i8m2_m(vbool4_t mask,vint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i8m2_m(mask,op1,op2,32);
}


vint8m4_t test___riscv_vmulhsu_vx_i8m4_m(vbool2_t mask,vint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i8m4_m(mask,op1,op2,32);
}


vint8m8_t test___riscv_vmulhsu_vx_i8m8_m(vbool1_t mask,vint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i8m8_m(mask,op1,op2,32);
}


vint16mf4_t test___riscv_vmulhsu_vx_i16mf4_m(vbool64_t mask,vint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i16mf4_m(mask,op1,op2,32);
}


vint16mf2_t test___riscv_vmulhsu_vx_i16mf2_m(vbool32_t mask,vint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i16mf2_m(mask,op1,op2,32);
}


vint16m1_t test___riscv_vmulhsu_vx_i16m1_m(vbool16_t mask,vint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i16m1_m(mask,op1,op2,32);
}


vint16m2_t test___riscv_vmulhsu_vx_i16m2_m(vbool8_t mask,vint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i16m2_m(mask,op1,op2,32);
}


vint16m4_t test___riscv_vmulhsu_vx_i16m4_m(vbool4_t mask,vint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i16m4_m(mask,op1,op2,32);
}


vint16m8_t test___riscv_vmulhsu_vx_i16m8_m(vbool2_t mask,vint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i16m8_m(mask,op1,op2,32);
}


vint32mf2_t test___riscv_vmulhsu_vx_i32mf2_m(vbool64_t mask,vint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i32mf2_m(mask,op1,op2,32);
}


vint32m1_t test___riscv_vmulhsu_vx_i32m1_m(vbool32_t mask,vint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i32m1_m(mask,op1,op2,32);
}


vint32m2_t test___riscv_vmulhsu_vx_i32m2_m(vbool16_t mask,vint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i32m2_m(mask,op1,op2,32);
}


vint32m4_t test___riscv_vmulhsu_vx_i32m4_m(vbool8_t mask,vint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i32m4_m(mask,op1,op2,32);
}


vint32m8_t test___riscv_vmulhsu_vx_i32m8_m(vbool4_t mask,vint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i32m8_m(mask,op1,op2,32);
}


vint64m1_t test___riscv_vmulhsu_vx_i64m1_m(vbool64_t mask,vint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i64m1_m(mask,op1,op2,32);
}


vint64m2_t test___riscv_vmulhsu_vx_i64m2_m(vbool32_t mask,vint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i64m2_m(mask,op1,op2,32);
}


vint64m4_t test___riscv_vmulhsu_vx_i64m4_m(vbool16_t mask,vint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i64m4_m(mask,op1,op2,32);
}


vint64m8_t test___riscv_vmulhsu_vx_i64m8_m(vbool8_t mask,vint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmulhsu_vx_i64m8_m(mask,op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmulhsu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vmulhsu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 4 } } */
