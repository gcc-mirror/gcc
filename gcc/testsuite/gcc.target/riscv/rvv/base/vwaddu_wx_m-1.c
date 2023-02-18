/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint16mf4_t test___riscv_vwaddu_wx_u16mf4_m(vbool64_t mask,vuint16mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u16mf4_m(mask,op1,0xAA,vl);
}


vuint16mf2_t test___riscv_vwaddu_wx_u16mf2_m(vbool32_t mask,vuint16mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u16mf2_m(mask,op1,0xAA,vl);
}


vuint16m1_t test___riscv_vwaddu_wx_u16m1_m(vbool16_t mask,vuint16m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u16m1_m(mask,op1,0xAA,vl);
}


vuint16m2_t test___riscv_vwaddu_wx_u16m2_m(vbool8_t mask,vuint16m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u16m2_m(mask,op1,0xAA,vl);
}


vuint16m4_t test___riscv_vwaddu_wx_u16m4_m(vbool4_t mask,vuint16m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u16m4_m(mask,op1,0xAA,vl);
}


vuint16m8_t test___riscv_vwaddu_wx_u16m8_m(vbool2_t mask,vuint16m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u16m8_m(mask,op1,0xAA,vl);
}


vuint32mf2_t test___riscv_vwaddu_wx_u32mf2_m(vbool64_t mask,vuint32mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u32mf2_m(mask,op1,0xAA,vl);
}


vuint32m1_t test___riscv_vwaddu_wx_u32m1_m(vbool32_t mask,vuint32m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u32m1_m(mask,op1,0xAA,vl);
}


vuint32m2_t test___riscv_vwaddu_wx_u32m2_m(vbool16_t mask,vuint32m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u32m2_m(mask,op1,0xAA,vl);
}


vuint32m4_t test___riscv_vwaddu_wx_u32m4_m(vbool8_t mask,vuint32m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u32m4_m(mask,op1,0xAA,vl);
}


vuint32m8_t test___riscv_vwaddu_wx_u32m8_m(vbool4_t mask,vuint32m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u32m8_m(mask,op1,0xAA,vl);
}


vuint64m1_t test___riscv_vwaddu_wx_u64m1_m(vbool64_t mask,vuint64m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u64m1_m(mask,op1,0xAA,vl);
}


vuint64m2_t test___riscv_vwaddu_wx_u64m2_m(vbool32_t mask,vuint64m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u64m2_m(mask,op1,0xAA,vl);
}


vuint64m4_t test___riscv_vwaddu_wx_u64m4_m(vbool16_t mask,vuint64m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u64m4_m(mask,op1,0xAA,vl);
}


vuint64m8_t test___riscv_vwaddu_wx_u64m8_m(vbool8_t mask,vuint64m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwaddu_wx_u64m8_m(mask,op1,0xAA,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwaddu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t} 1 } } */
