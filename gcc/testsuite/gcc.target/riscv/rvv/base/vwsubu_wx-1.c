/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint16mf4_t test___riscv_vwsubu_wx_u16mf4(vuint16mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u16mf4(op1,0xAA,vl);
}


vuint16mf2_t test___riscv_vwsubu_wx_u16mf2(vuint16mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u16mf2(op1,0xAA,vl);
}


vuint16m1_t test___riscv_vwsubu_wx_u16m1(vuint16m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u16m1(op1,0xAA,vl);
}


vuint16m2_t test___riscv_vwsubu_wx_u16m2(vuint16m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u16m2(op1,0xAA,vl);
}


vuint16m4_t test___riscv_vwsubu_wx_u16m4(vuint16m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u16m4(op1,0xAA,vl);
}


vuint16m8_t test___riscv_vwsubu_wx_u16m8(vuint16m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u16m8(op1,0xAA,vl);
}


vuint32mf2_t test___riscv_vwsubu_wx_u32mf2(vuint32mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u32mf2(op1,0xAA,vl);
}


vuint32m1_t test___riscv_vwsubu_wx_u32m1(vuint32m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u32m1(op1,0xAA,vl);
}


vuint32m2_t test___riscv_vwsubu_wx_u32m2(vuint32m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u32m2(op1,0xAA,vl);
}


vuint32m4_t test___riscv_vwsubu_wx_u32m4(vuint32m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u32m4(op1,0xAA,vl);
}


vuint32m8_t test___riscv_vwsubu_wx_u32m8(vuint32m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u32m8(op1,0xAA,vl);
}


vuint64m1_t test___riscv_vwsubu_wx_u64m1(vuint64m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u64m1(op1,0xAA,vl);
}


vuint64m2_t test___riscv_vwsubu_wx_u64m2(vuint64m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u64m2(op1,0xAA,vl);
}


vuint64m4_t test___riscv_vwsubu_wx_u64m4(vuint64m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u64m4(op1,0xAA,vl);
}


vuint64m8_t test___riscv_vwsubu_wx_u64m8(vuint64m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vwsubu_wx_u64m8(op1,0xAA,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwsubu\.wx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
