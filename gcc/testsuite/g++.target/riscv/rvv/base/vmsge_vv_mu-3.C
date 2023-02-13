/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool64_t test___riscv_vmsge_mu(vbool64_t mask,vbool64_t merge,vint8mf8_t op1,vint8mf8_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool32_t test___riscv_vmsge_mu(vbool32_t mask,vbool32_t merge,vint8mf4_t op1,vint8mf4_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool16_t test___riscv_vmsge_mu(vbool16_t mask,vbool16_t merge,vint8mf2_t op1,vint8mf2_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool8_t test___riscv_vmsge_mu(vbool8_t mask,vbool8_t merge,vint8m1_t op1,vint8m1_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool4_t test___riscv_vmsge_mu(vbool4_t mask,vbool4_t merge,vint8m2_t op1,vint8m2_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool2_t test___riscv_vmsge_mu(vbool2_t mask,vbool2_t merge,vint8m4_t op1,vint8m4_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool1_t test___riscv_vmsge_mu(vbool1_t mask,vbool1_t merge,vint8m8_t op1,vint8m8_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool64_t test___riscv_vmsge_mu(vbool64_t mask,vbool64_t merge,vint16mf4_t op1,vint16mf4_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool32_t test___riscv_vmsge_mu(vbool32_t mask,vbool32_t merge,vint16mf2_t op1,vint16mf2_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool16_t test___riscv_vmsge_mu(vbool16_t mask,vbool16_t merge,vint16m1_t op1,vint16m1_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool8_t test___riscv_vmsge_mu(vbool8_t mask,vbool8_t merge,vint16m2_t op1,vint16m2_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool4_t test___riscv_vmsge_mu(vbool4_t mask,vbool4_t merge,vint16m4_t op1,vint16m4_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool2_t test___riscv_vmsge_mu(vbool2_t mask,vbool2_t merge,vint16m8_t op1,vint16m8_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool64_t test___riscv_vmsge_mu(vbool64_t mask,vbool64_t merge,vint32mf2_t op1,vint32mf2_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool32_t test___riscv_vmsge_mu(vbool32_t mask,vbool32_t merge,vint32m1_t op1,vint32m1_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool16_t test___riscv_vmsge_mu(vbool16_t mask,vbool16_t merge,vint32m2_t op1,vint32m2_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool8_t test___riscv_vmsge_mu(vbool8_t mask,vbool8_t merge,vint32m4_t op1,vint32m4_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool4_t test___riscv_vmsge_mu(vbool4_t mask,vbool4_t merge,vint32m8_t op1,vint32m8_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool64_t test___riscv_vmsge_mu(vbool64_t mask,vbool64_t merge,vint64m1_t op1,vint64m1_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool32_t test___riscv_vmsge_mu(vbool32_t mask,vbool32_t merge,vint64m2_t op1,vint64m2_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool16_t test___riscv_vmsge_mu(vbool16_t mask,vbool16_t merge,vint64m4_t op1,vint64m4_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}


vbool8_t test___riscv_vmsge_mu(vbool8_t mask,vbool8_t merge,vint64m8_t op1,vint64m8_t op2,size_t vl)
{
    return __riscv_vmsge_mu(mask,merge,op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*mu\s+vmsge\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
