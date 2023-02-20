/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool64_t test___riscv_vmsltu_mu(vbool64_t mask,vbool64_t merge,vuint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool32_t test___riscv_vmsltu_mu(vbool32_t mask,vbool32_t merge,vuint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool16_t test___riscv_vmsltu_mu(vbool16_t mask,vbool16_t merge,vuint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool8_t test___riscv_vmsltu_mu(vbool8_t mask,vbool8_t merge,vuint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool4_t test___riscv_vmsltu_mu(vbool4_t mask,vbool4_t merge,vuint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool2_t test___riscv_vmsltu_mu(vbool2_t mask,vbool2_t merge,vuint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool1_t test___riscv_vmsltu_mu(vbool1_t mask,vbool1_t merge,vuint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool64_t test___riscv_vmsltu_mu(vbool64_t mask,vbool64_t merge,vuint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool32_t test___riscv_vmsltu_mu(vbool32_t mask,vbool32_t merge,vuint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool16_t test___riscv_vmsltu_mu(vbool16_t mask,vbool16_t merge,vuint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool8_t test___riscv_vmsltu_mu(vbool8_t mask,vbool8_t merge,vuint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool4_t test___riscv_vmsltu_mu(vbool4_t mask,vbool4_t merge,vuint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool2_t test___riscv_vmsltu_mu(vbool2_t mask,vbool2_t merge,vuint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool64_t test___riscv_vmsltu_mu(vbool64_t mask,vbool64_t merge,vuint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool32_t test___riscv_vmsltu_mu(vbool32_t mask,vbool32_t merge,vuint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool16_t test___riscv_vmsltu_mu(vbool16_t mask,vbool16_t merge,vuint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool8_t test___riscv_vmsltu_mu(vbool8_t mask,vbool8_t merge,vuint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool4_t test___riscv_vmsltu_mu(vbool4_t mask,vbool4_t merge,vuint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool64_t test___riscv_vmsltu_mu(vbool64_t mask,vbool64_t merge,vuint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool32_t test___riscv_vmsltu_mu(vbool32_t mask,vbool32_t merge,vuint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool16_t test___riscv_vmsltu_mu(vbool16_t mask,vbool16_t merge,vuint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}


vbool8_t test___riscv_vmsltu_mu(vbool8_t mask,vbool8_t merge,vuint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmsltu_mu(mask,merge,op1,op2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*mu\s+vmsltu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vmsltu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 4 } } */
