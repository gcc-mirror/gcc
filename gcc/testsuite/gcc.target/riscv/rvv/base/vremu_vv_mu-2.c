/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint8mf8_t test___riscv_vremu_vv_u8mf8_mu(vbool64_t mask,vuint8mf8_t merge,vuint8mf8_t op1,vuint8mf8_t op2,size_t vl)
{
    return __riscv_vremu_vv_u8mf8_mu(mask,merge,op1,op2,31);
}


vuint8mf4_t test___riscv_vremu_vv_u8mf4_mu(vbool32_t mask,vuint8mf4_t merge,vuint8mf4_t op1,vuint8mf4_t op2,size_t vl)
{
    return __riscv_vremu_vv_u8mf4_mu(mask,merge,op1,op2,31);
}


vuint8mf2_t test___riscv_vremu_vv_u8mf2_mu(vbool16_t mask,vuint8mf2_t merge,vuint8mf2_t op1,vuint8mf2_t op2,size_t vl)
{
    return __riscv_vremu_vv_u8mf2_mu(mask,merge,op1,op2,31);
}


vuint8m1_t test___riscv_vremu_vv_u8m1_mu(vbool8_t mask,vuint8m1_t merge,vuint8m1_t op1,vuint8m1_t op2,size_t vl)
{
    return __riscv_vremu_vv_u8m1_mu(mask,merge,op1,op2,31);
}


vuint8m2_t test___riscv_vremu_vv_u8m2_mu(vbool4_t mask,vuint8m2_t merge,vuint8m2_t op1,vuint8m2_t op2,size_t vl)
{
    return __riscv_vremu_vv_u8m2_mu(mask,merge,op1,op2,31);
}


vuint8m4_t test___riscv_vremu_vv_u8m4_mu(vbool2_t mask,vuint8m4_t merge,vuint8m4_t op1,vuint8m4_t op2,size_t vl)
{
    return __riscv_vremu_vv_u8m4_mu(mask,merge,op1,op2,31);
}


vuint8m8_t test___riscv_vremu_vv_u8m8_mu(vbool1_t mask,vuint8m8_t merge,vuint8m8_t op1,vuint8m8_t op2,size_t vl)
{
    return __riscv_vremu_vv_u8m8_mu(mask,merge,op1,op2,31);
}


vuint16mf4_t test___riscv_vremu_vv_u16mf4_mu(vbool64_t mask,vuint16mf4_t merge,vuint16mf4_t op1,vuint16mf4_t op2,size_t vl)
{
    return __riscv_vremu_vv_u16mf4_mu(mask,merge,op1,op2,31);
}


vuint16mf2_t test___riscv_vremu_vv_u16mf2_mu(vbool32_t mask,vuint16mf2_t merge,vuint16mf2_t op1,vuint16mf2_t op2,size_t vl)
{
    return __riscv_vremu_vv_u16mf2_mu(mask,merge,op1,op2,31);
}


vuint16m1_t test___riscv_vremu_vv_u16m1_mu(vbool16_t mask,vuint16m1_t merge,vuint16m1_t op1,vuint16m1_t op2,size_t vl)
{
    return __riscv_vremu_vv_u16m1_mu(mask,merge,op1,op2,31);
}


vuint16m2_t test___riscv_vremu_vv_u16m2_mu(vbool8_t mask,vuint16m2_t merge,vuint16m2_t op1,vuint16m2_t op2,size_t vl)
{
    return __riscv_vremu_vv_u16m2_mu(mask,merge,op1,op2,31);
}


vuint16m4_t test___riscv_vremu_vv_u16m4_mu(vbool4_t mask,vuint16m4_t merge,vuint16m4_t op1,vuint16m4_t op2,size_t vl)
{
    return __riscv_vremu_vv_u16m4_mu(mask,merge,op1,op2,31);
}


vuint16m8_t test___riscv_vremu_vv_u16m8_mu(vbool2_t mask,vuint16m8_t merge,vuint16m8_t op1,vuint16m8_t op2,size_t vl)
{
    return __riscv_vremu_vv_u16m8_mu(mask,merge,op1,op2,31);
}


vuint32mf2_t test___riscv_vremu_vv_u32mf2_mu(vbool64_t mask,vuint32mf2_t merge,vuint32mf2_t op1,vuint32mf2_t op2,size_t vl)
{
    return __riscv_vremu_vv_u32mf2_mu(mask,merge,op1,op2,31);
}


vuint32m1_t test___riscv_vremu_vv_u32m1_mu(vbool32_t mask,vuint32m1_t merge,vuint32m1_t op1,vuint32m1_t op2,size_t vl)
{
    return __riscv_vremu_vv_u32m1_mu(mask,merge,op1,op2,31);
}


vuint32m2_t test___riscv_vremu_vv_u32m2_mu(vbool16_t mask,vuint32m2_t merge,vuint32m2_t op1,vuint32m2_t op2,size_t vl)
{
    return __riscv_vremu_vv_u32m2_mu(mask,merge,op1,op2,31);
}


vuint32m4_t test___riscv_vremu_vv_u32m4_mu(vbool8_t mask,vuint32m4_t merge,vuint32m4_t op1,vuint32m4_t op2,size_t vl)
{
    return __riscv_vremu_vv_u32m4_mu(mask,merge,op1,op2,31);
}


vuint32m8_t test___riscv_vremu_vv_u32m8_mu(vbool4_t mask,vuint32m8_t merge,vuint32m8_t op1,vuint32m8_t op2,size_t vl)
{
    return __riscv_vremu_vv_u32m8_mu(mask,merge,op1,op2,31);
}


vuint64m1_t test___riscv_vremu_vv_u64m1_mu(vbool64_t mask,vuint64m1_t merge,vuint64m1_t op1,vuint64m1_t op2,size_t vl)
{
    return __riscv_vremu_vv_u64m1_mu(mask,merge,op1,op2,31);
}


vuint64m2_t test___riscv_vremu_vv_u64m2_mu(vbool32_t mask,vuint64m2_t merge,vuint64m2_t op1,vuint64m2_t op2,size_t vl)
{
    return __riscv_vremu_vv_u64m2_mu(mask,merge,op1,op2,31);
}


vuint64m4_t test___riscv_vremu_vv_u64m4_mu(vbool16_t mask,vuint64m4_t merge,vuint64m4_t op1,vuint64m4_t op2,size_t vl)
{
    return __riscv_vremu_vv_u64m4_mu(mask,merge,op1,op2,31);
}


vuint64m8_t test___riscv_vremu_vv_u64m8_mu(vbool8_t mask,vuint64m8_t merge,vuint64m8_t op1,vuint64m8_t op2,size_t vl)
{
    return __riscv_vremu_vv_u64m8_mu(mask,merge,op1,op2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*mu\s+vremu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
