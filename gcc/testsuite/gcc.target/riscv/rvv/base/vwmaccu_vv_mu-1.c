/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint16mf4_t test___riscv_vwmaccu_vv_u16mf4_mu(vbool64_t mask,vuint16mf4_t vd,vuint8mf8_t vs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16mf4_mu(mask,vd,vs1,vs2,vl);
}


vuint16mf2_t test___riscv_vwmaccu_vv_u16mf2_mu(vbool32_t mask,vuint16mf2_t vd,vuint8mf4_t vs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16mf2_mu(mask,vd,vs1,vs2,vl);
}


vuint16m1_t test___riscv_vwmaccu_vv_u16m1_mu(vbool16_t mask,vuint16m1_t vd,vuint8mf2_t vs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16m1_mu(mask,vd,vs1,vs2,vl);
}


vuint16m2_t test___riscv_vwmaccu_vv_u16m2_mu(vbool8_t mask,vuint16m2_t vd,vuint8m1_t vs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16m2_mu(mask,vd,vs1,vs2,vl);
}


vuint16m4_t test___riscv_vwmaccu_vv_u16m4_mu(vbool4_t mask,vuint16m4_t vd,vuint8m2_t vs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16m4_mu(mask,vd,vs1,vs2,vl);
}


vuint16m8_t test___riscv_vwmaccu_vv_u16m8_mu(vbool2_t mask,vuint16m8_t vd,vuint8m4_t vs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16m8_mu(mask,vd,vs1,vs2,vl);
}


vuint32mf2_t test___riscv_vwmaccu_vv_u32mf2_mu(vbool64_t mask,vuint32mf2_t vd,vuint16mf4_t vs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u32mf2_mu(mask,vd,vs1,vs2,vl);
}


vuint32m1_t test___riscv_vwmaccu_vv_u32m1_mu(vbool32_t mask,vuint32m1_t vd,vuint16mf2_t vs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u32m1_mu(mask,vd,vs1,vs2,vl);
}


vuint32m2_t test___riscv_vwmaccu_vv_u32m2_mu(vbool16_t mask,vuint32m2_t vd,vuint16m1_t vs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u32m2_mu(mask,vd,vs1,vs2,vl);
}


vuint32m4_t test___riscv_vwmaccu_vv_u32m4_mu(vbool8_t mask,vuint32m4_t vd,vuint16m2_t vs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u32m4_mu(mask,vd,vs1,vs2,vl);
}


vuint32m8_t test___riscv_vwmaccu_vv_u32m8_mu(vbool4_t mask,vuint32m8_t vd,vuint16m4_t vs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u32m8_mu(mask,vd,vs1,vs2,vl);
}


vuint64m1_t test___riscv_vwmaccu_vv_u64m1_mu(vbool64_t mask,vuint64m1_t vd,vuint32mf2_t vs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u64m1_mu(mask,vd,vs1,vs2,vl);
}


vuint64m2_t test___riscv_vwmaccu_vv_u64m2_mu(vbool32_t mask,vuint64m2_t vd,vuint32m1_t vs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u64m2_mu(mask,vd,vs1,vs2,vl);
}


vuint64m4_t test___riscv_vwmaccu_vv_u64m4_mu(vbool16_t mask,vuint64m4_t vd,vuint32m2_t vs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u64m4_mu(mask,vd,vs1,vs2,vl);
}


vuint64m8_t test___riscv_vwmaccu_vv_u64m8_mu(vbool8_t mask,vuint64m8_t vd,vuint32m4_t vs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u64m8_mu(mask,vd,vs1,vs2,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*mu\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
