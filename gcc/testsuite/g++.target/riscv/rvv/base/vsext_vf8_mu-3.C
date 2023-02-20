/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint64m1_t test___riscv_vsext_vf8_mu(vbool64_t mask,vint64m1_t merge,vint8mf8_t op1,size_t vl)
{
    return __riscv_vsext_vf8_mu(mask,merge,op1,32);
}


vint64m2_t test___riscv_vsext_vf8_mu(vbool32_t mask,vint64m2_t merge,vint8mf4_t op1,size_t vl)
{
    return __riscv_vsext_vf8_mu(mask,merge,op1,32);
}


vint64m4_t test___riscv_vsext_vf8_mu(vbool16_t mask,vint64m4_t merge,vint8mf2_t op1,size_t vl)
{
    return __riscv_vsext_vf8_mu(mask,merge,op1,32);
}


vint64m8_t test___riscv_vsext_vf8_mu(vbool8_t mask,vint64m8_t merge,vint8m1_t op1,size_t vl)
{
    return __riscv_vsext_vf8_mu(mask,merge,op1,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*mu\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*mu\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*mu\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*mu\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
