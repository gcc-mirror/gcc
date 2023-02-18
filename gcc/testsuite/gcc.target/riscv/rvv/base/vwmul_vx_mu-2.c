/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmul_vx_i16mf4_mu(vbool64_t mask,vint16mf4_t merge,vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i16mf4_mu(mask,merge,op1,op2,31);
}


vint16mf2_t test___riscv_vwmul_vx_i16mf2_mu(vbool32_t mask,vint16mf2_t merge,vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i16mf2_mu(mask,merge,op1,op2,31);
}


vint16m1_t test___riscv_vwmul_vx_i16m1_mu(vbool16_t mask,vint16m1_t merge,vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i16m1_mu(mask,merge,op1,op2,31);
}


vint16m2_t test___riscv_vwmul_vx_i16m2_mu(vbool8_t mask,vint16m2_t merge,vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i16m2_mu(mask,merge,op1,op2,31);
}


vint16m4_t test___riscv_vwmul_vx_i16m4_mu(vbool4_t mask,vint16m4_t merge,vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i16m4_mu(mask,merge,op1,op2,31);
}


vint16m8_t test___riscv_vwmul_vx_i16m8_mu(vbool2_t mask,vint16m8_t merge,vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i16m8_mu(mask,merge,op1,op2,31);
}


vint32mf2_t test___riscv_vwmul_vx_i32mf2_mu(vbool64_t mask,vint32mf2_t merge,vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i32mf2_mu(mask,merge,op1,op2,31);
}


vint32m1_t test___riscv_vwmul_vx_i32m1_mu(vbool32_t mask,vint32m1_t merge,vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i32m1_mu(mask,merge,op1,op2,31);
}


vint32m2_t test___riscv_vwmul_vx_i32m2_mu(vbool16_t mask,vint32m2_t merge,vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i32m2_mu(mask,merge,op1,op2,31);
}


vint32m4_t test___riscv_vwmul_vx_i32m4_mu(vbool8_t mask,vint32m4_t merge,vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i32m4_mu(mask,merge,op1,op2,31);
}


vint32m8_t test___riscv_vwmul_vx_i32m8_mu(vbool4_t mask,vint32m8_t merge,vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i32m8_mu(mask,merge,op1,op2,31);
}


vint64m1_t test___riscv_vwmul_vx_i64m1_mu(vbool64_t mask,vint64m1_t merge,vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i64m1_mu(mask,merge,op1,op2,31);
}


vint64m2_t test___riscv_vwmul_vx_i64m2_mu(vbool32_t mask,vint64m2_t merge,vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i64m2_mu(mask,merge,op1,op2,31);
}


vint64m4_t test___riscv_vwmul_vx_i64m4_mu(vbool16_t mask,vint64m4_t merge,vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i64m4_mu(mask,merge,op1,op2,31);
}


vint64m8_t test___riscv_vwmul_vx_i64m8_mu(vbool8_t mask,vint64m8_t merge,vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vwmul_vx_i64m8_mu(mask,merge,op1,op2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*mu\s+vwmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
