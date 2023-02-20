/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool64_t test___riscv_vmsltu_vv_u8mf8_b64_mu(vbool64_t mask,vbool64_t merge,vuint8mf8_t op1,vuint8mf8_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u8mf8_b64_mu(mask,merge,op1,op2,32);
}


vbool32_t test___riscv_vmsltu_vv_u8mf4_b32_mu(vbool32_t mask,vbool32_t merge,vuint8mf4_t op1,vuint8mf4_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u8mf4_b32_mu(mask,merge,op1,op2,32);
}


vbool16_t test___riscv_vmsltu_vv_u8mf2_b16_mu(vbool16_t mask,vbool16_t merge,vuint8mf2_t op1,vuint8mf2_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u8mf2_b16_mu(mask,merge,op1,op2,32);
}


vbool8_t test___riscv_vmsltu_vv_u8m1_b8_mu(vbool8_t mask,vbool8_t merge,vuint8m1_t op1,vuint8m1_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u8m1_b8_mu(mask,merge,op1,op2,32);
}


vbool4_t test___riscv_vmsltu_vv_u8m2_b4_mu(vbool4_t mask,vbool4_t merge,vuint8m2_t op1,vuint8m2_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u8m2_b4_mu(mask,merge,op1,op2,32);
}


vbool2_t test___riscv_vmsltu_vv_u8m4_b2_mu(vbool2_t mask,vbool2_t merge,vuint8m4_t op1,vuint8m4_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u8m4_b2_mu(mask,merge,op1,op2,32);
}


vbool1_t test___riscv_vmsltu_vv_u8m8_b1_mu(vbool1_t mask,vbool1_t merge,vuint8m8_t op1,vuint8m8_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u8m8_b1_mu(mask,merge,op1,op2,32);
}


vbool64_t test___riscv_vmsltu_vv_u16mf4_b64_mu(vbool64_t mask,vbool64_t merge,vuint16mf4_t op1,vuint16mf4_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u16mf4_b64_mu(mask,merge,op1,op2,32);
}


vbool32_t test___riscv_vmsltu_vv_u16mf2_b32_mu(vbool32_t mask,vbool32_t merge,vuint16mf2_t op1,vuint16mf2_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u16mf2_b32_mu(mask,merge,op1,op2,32);
}


vbool16_t test___riscv_vmsltu_vv_u16m1_b16_mu(vbool16_t mask,vbool16_t merge,vuint16m1_t op1,vuint16m1_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u16m1_b16_mu(mask,merge,op1,op2,32);
}


vbool8_t test___riscv_vmsltu_vv_u16m2_b8_mu(vbool8_t mask,vbool8_t merge,vuint16m2_t op1,vuint16m2_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u16m2_b8_mu(mask,merge,op1,op2,32);
}


vbool4_t test___riscv_vmsltu_vv_u16m4_b4_mu(vbool4_t mask,vbool4_t merge,vuint16m4_t op1,vuint16m4_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u16m4_b4_mu(mask,merge,op1,op2,32);
}


vbool2_t test___riscv_vmsltu_vv_u16m8_b2_mu(vbool2_t mask,vbool2_t merge,vuint16m8_t op1,vuint16m8_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u16m8_b2_mu(mask,merge,op1,op2,32);
}


vbool64_t test___riscv_vmsltu_vv_u32mf2_b64_mu(vbool64_t mask,vbool64_t merge,vuint32mf2_t op1,vuint32mf2_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u32mf2_b64_mu(mask,merge,op1,op2,32);
}


vbool32_t test___riscv_vmsltu_vv_u32m1_b32_mu(vbool32_t mask,vbool32_t merge,vuint32m1_t op1,vuint32m1_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u32m1_b32_mu(mask,merge,op1,op2,32);
}


vbool16_t test___riscv_vmsltu_vv_u32m2_b16_mu(vbool16_t mask,vbool16_t merge,vuint32m2_t op1,vuint32m2_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u32m2_b16_mu(mask,merge,op1,op2,32);
}


vbool8_t test___riscv_vmsltu_vv_u32m4_b8_mu(vbool8_t mask,vbool8_t merge,vuint32m4_t op1,vuint32m4_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u32m4_b8_mu(mask,merge,op1,op2,32);
}


vbool4_t test___riscv_vmsltu_vv_u32m8_b4_mu(vbool4_t mask,vbool4_t merge,vuint32m8_t op1,vuint32m8_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u32m8_b4_mu(mask,merge,op1,op2,32);
}


vbool64_t test___riscv_vmsltu_vv_u64m1_b64_mu(vbool64_t mask,vbool64_t merge,vuint64m1_t op1,vuint64m1_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u64m1_b64_mu(mask,merge,op1,op2,32);
}


vbool32_t test___riscv_vmsltu_vv_u64m2_b32_mu(vbool32_t mask,vbool32_t merge,vuint64m2_t op1,vuint64m2_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u64m2_b32_mu(mask,merge,op1,op2,32);
}


vbool16_t test___riscv_vmsltu_vv_u64m4_b16_mu(vbool16_t mask,vbool16_t merge,vuint64m4_t op1,vuint64m4_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u64m4_b16_mu(mask,merge,op1,op2,32);
}


vbool8_t test___riscv_vmsltu_vv_u64m8_b8_mu(vbool8_t mask,vbool8_t merge,vuint64m8_t op1,vuint64m8_t op2,size_t vl)
{
    return __riscv_vmsltu_vv_u64m8_b8_mu(mask,merge,op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*mu\s+vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
