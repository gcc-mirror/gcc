/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmul_vv_i16mf4(vint8mf8_t op1,vint8mf8_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i16mf4(op1,op2,vl);
}


vint16mf2_t test___riscv_vwmul_vv_i16mf2(vint8mf4_t op1,vint8mf4_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i16mf2(op1,op2,vl);
}


vint16m1_t test___riscv_vwmul_vv_i16m1(vint8mf2_t op1,vint8mf2_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i16m1(op1,op2,vl);
}


vint16m2_t test___riscv_vwmul_vv_i16m2(vint8m1_t op1,vint8m1_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i16m2(op1,op2,vl);
}


vint16m4_t test___riscv_vwmul_vv_i16m4(vint8m2_t op1,vint8m2_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i16m4(op1,op2,vl);
}


vint16m8_t test___riscv_vwmul_vv_i16m8(vint8m4_t op1,vint8m4_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i16m8(op1,op2,vl);
}


vint32mf2_t test___riscv_vwmul_vv_i32mf2(vint16mf4_t op1,vint16mf4_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i32mf2(op1,op2,vl);
}


vint32m1_t test___riscv_vwmul_vv_i32m1(vint16mf2_t op1,vint16mf2_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i32m1(op1,op2,vl);
}


vint32m2_t test___riscv_vwmul_vv_i32m2(vint16m1_t op1,vint16m1_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i32m2(op1,op2,vl);
}


vint32m4_t test___riscv_vwmul_vv_i32m4(vint16m2_t op1,vint16m2_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i32m4(op1,op2,vl);
}


vint32m8_t test___riscv_vwmul_vv_i32m8(vint16m4_t op1,vint16m4_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i32m8(op1,op2,vl);
}


vint64m1_t test___riscv_vwmul_vv_i64m1(vint32mf2_t op1,vint32mf2_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i64m1(op1,op2,vl);
}


vint64m2_t test___riscv_vwmul_vv_i64m2(vint32m1_t op1,vint32m1_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i64m2(op1,op2,vl);
}


vint64m4_t test___riscv_vwmul_vv_i64m4(vint32m2_t op1,vint32m2_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i64m4(op1,op2,vl);
}


vint64m8_t test___riscv_vwmul_vv_i64m8(vint32m4_t op1,vint32m4_t op2,size_t vl)
{
    return __riscv_vwmul_vv_i64m8(op1,op2,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
