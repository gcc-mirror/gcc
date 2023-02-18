/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint16mf4_t test___riscv_vwmaccu_vx_u16mf4_mu(vbool64_t mask,vuint16mf4_t vd,uint8_t rs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u16mf4_mu(mask,vd,rs1,vs2,31);
}


vuint16mf2_t test___riscv_vwmaccu_vx_u16mf2_mu(vbool32_t mask,vuint16mf2_t vd,uint8_t rs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u16mf2_mu(mask,vd,rs1,vs2,31);
}


vuint16m1_t test___riscv_vwmaccu_vx_u16m1_mu(vbool16_t mask,vuint16m1_t vd,uint8_t rs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u16m1_mu(mask,vd,rs1,vs2,31);
}


vuint16m2_t test___riscv_vwmaccu_vx_u16m2_mu(vbool8_t mask,vuint16m2_t vd,uint8_t rs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u16m2_mu(mask,vd,rs1,vs2,31);
}


vuint16m4_t test___riscv_vwmaccu_vx_u16m4_mu(vbool4_t mask,vuint16m4_t vd,uint8_t rs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u16m4_mu(mask,vd,rs1,vs2,31);
}


vuint16m8_t test___riscv_vwmaccu_vx_u16m8_mu(vbool2_t mask,vuint16m8_t vd,uint8_t rs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u16m8_mu(mask,vd,rs1,vs2,31);
}


vuint32mf2_t test___riscv_vwmaccu_vx_u32mf2_mu(vbool64_t mask,vuint32mf2_t vd,uint16_t rs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u32mf2_mu(mask,vd,rs1,vs2,31);
}


vuint32m1_t test___riscv_vwmaccu_vx_u32m1_mu(vbool32_t mask,vuint32m1_t vd,uint16_t rs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u32m1_mu(mask,vd,rs1,vs2,31);
}


vuint32m2_t test___riscv_vwmaccu_vx_u32m2_mu(vbool16_t mask,vuint32m2_t vd,uint16_t rs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u32m2_mu(mask,vd,rs1,vs2,31);
}


vuint32m4_t test___riscv_vwmaccu_vx_u32m4_mu(vbool8_t mask,vuint32m4_t vd,uint16_t rs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u32m4_mu(mask,vd,rs1,vs2,31);
}


vuint32m8_t test___riscv_vwmaccu_vx_u32m8_mu(vbool4_t mask,vuint32m8_t vd,uint16_t rs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u32m8_mu(mask,vd,rs1,vs2,31);
}


vuint64m1_t test___riscv_vwmaccu_vx_u64m1_mu(vbool64_t mask,vuint64m1_t vd,uint32_t rs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u64m1_mu(mask,vd,rs1,vs2,31);
}


vuint64m2_t test___riscv_vwmaccu_vx_u64m2_mu(vbool32_t mask,vuint64m2_t vd,uint32_t rs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u64m2_mu(mask,vd,rs1,vs2,31);
}


vuint64m4_t test___riscv_vwmaccu_vx_u64m4_mu(vbool16_t mask,vuint64m4_t vd,uint32_t rs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u64m4_mu(mask,vd,rs1,vs2,31);
}


vuint64m8_t test___riscv_vwmaccu_vx_u64m8_mu(vbool8_t mask,vuint64m8_t vd,uint32_t rs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vx_u64m8_mu(mask,vd,rs1,vs2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*mu\s+vwmaccu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
