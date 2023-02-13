/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool64_t test___riscv_vmsgt_vv_i8mf8_b64_mu(vbool64_t mask,vbool64_t merge,vint8mf8_t op1,vint8mf8_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i8mf8_b64_mu(mask,merge,op1,op2,31);
}


vbool32_t test___riscv_vmsgt_vv_i8mf4_b32_mu(vbool32_t mask,vbool32_t merge,vint8mf4_t op1,vint8mf4_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i8mf4_b32_mu(mask,merge,op1,op2,31);
}


vbool16_t test___riscv_vmsgt_vv_i8mf2_b16_mu(vbool16_t mask,vbool16_t merge,vint8mf2_t op1,vint8mf2_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i8mf2_b16_mu(mask,merge,op1,op2,31);
}


vbool8_t test___riscv_vmsgt_vv_i8m1_b8_mu(vbool8_t mask,vbool8_t merge,vint8m1_t op1,vint8m1_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i8m1_b8_mu(mask,merge,op1,op2,31);
}


vbool4_t test___riscv_vmsgt_vv_i8m2_b4_mu(vbool4_t mask,vbool4_t merge,vint8m2_t op1,vint8m2_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i8m2_b4_mu(mask,merge,op1,op2,31);
}


vbool2_t test___riscv_vmsgt_vv_i8m4_b2_mu(vbool2_t mask,vbool2_t merge,vint8m4_t op1,vint8m4_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i8m4_b2_mu(mask,merge,op1,op2,31);
}


vbool1_t test___riscv_vmsgt_vv_i8m8_b1_mu(vbool1_t mask,vbool1_t merge,vint8m8_t op1,vint8m8_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i8m8_b1_mu(mask,merge,op1,op2,31);
}


vbool64_t test___riscv_vmsgt_vv_i16mf4_b64_mu(vbool64_t mask,vbool64_t merge,vint16mf4_t op1,vint16mf4_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i16mf4_b64_mu(mask,merge,op1,op2,31);
}


vbool32_t test___riscv_vmsgt_vv_i16mf2_b32_mu(vbool32_t mask,vbool32_t merge,vint16mf2_t op1,vint16mf2_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i16mf2_b32_mu(mask,merge,op1,op2,31);
}


vbool16_t test___riscv_vmsgt_vv_i16m1_b16_mu(vbool16_t mask,vbool16_t merge,vint16m1_t op1,vint16m1_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i16m1_b16_mu(mask,merge,op1,op2,31);
}


vbool8_t test___riscv_vmsgt_vv_i16m2_b8_mu(vbool8_t mask,vbool8_t merge,vint16m2_t op1,vint16m2_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i16m2_b8_mu(mask,merge,op1,op2,31);
}


vbool4_t test___riscv_vmsgt_vv_i16m4_b4_mu(vbool4_t mask,vbool4_t merge,vint16m4_t op1,vint16m4_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i16m4_b4_mu(mask,merge,op1,op2,31);
}


vbool2_t test___riscv_vmsgt_vv_i16m8_b2_mu(vbool2_t mask,vbool2_t merge,vint16m8_t op1,vint16m8_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i16m8_b2_mu(mask,merge,op1,op2,31);
}


vbool64_t test___riscv_vmsgt_vv_i32mf2_b64_mu(vbool64_t mask,vbool64_t merge,vint32mf2_t op1,vint32mf2_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i32mf2_b64_mu(mask,merge,op1,op2,31);
}


vbool32_t test___riscv_vmsgt_vv_i32m1_b32_mu(vbool32_t mask,vbool32_t merge,vint32m1_t op1,vint32m1_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i32m1_b32_mu(mask,merge,op1,op2,31);
}


vbool16_t test___riscv_vmsgt_vv_i32m2_b16_mu(vbool16_t mask,vbool16_t merge,vint32m2_t op1,vint32m2_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i32m2_b16_mu(mask,merge,op1,op2,31);
}


vbool8_t test___riscv_vmsgt_vv_i32m4_b8_mu(vbool8_t mask,vbool8_t merge,vint32m4_t op1,vint32m4_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i32m4_b8_mu(mask,merge,op1,op2,31);
}


vbool4_t test___riscv_vmsgt_vv_i32m8_b4_mu(vbool4_t mask,vbool4_t merge,vint32m8_t op1,vint32m8_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i32m8_b4_mu(mask,merge,op1,op2,31);
}


vbool64_t test___riscv_vmsgt_vv_i64m1_b64_mu(vbool64_t mask,vbool64_t merge,vint64m1_t op1,vint64m1_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i64m1_b64_mu(mask,merge,op1,op2,31);
}


vbool32_t test___riscv_vmsgt_vv_i64m2_b32_mu(vbool32_t mask,vbool32_t merge,vint64m2_t op1,vint64m2_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i64m2_b32_mu(mask,merge,op1,op2,31);
}


vbool16_t test___riscv_vmsgt_vv_i64m4_b16_mu(vbool16_t mask,vbool16_t merge,vint64m4_t op1,vint64m4_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i64m4_b16_mu(mask,merge,op1,op2,31);
}


vbool8_t test___riscv_vmsgt_vv_i64m8_b8_mu(vbool8_t mask,vbool8_t merge,vint64m8_t op1,vint64m8_t op2,size_t vl)
{
    return __riscv_vmsgt_vv_i64m8_b8_mu(mask,merge,op1,op2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*mu\s+vmsgt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
