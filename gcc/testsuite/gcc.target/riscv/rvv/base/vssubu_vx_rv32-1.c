/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint8mf8_t test___riscv_vssubu_vx_u8mf8(vuint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8mf8(op1,op2,vl);
}


vuint8mf4_t test___riscv_vssubu_vx_u8mf4(vuint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8mf4(op1,op2,vl);
}


vuint8mf2_t test___riscv_vssubu_vx_u8mf2(vuint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8mf2(op1,op2,vl);
}


vuint8m1_t test___riscv_vssubu_vx_u8m1(vuint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8m1(op1,op2,vl);
}


vuint8m2_t test___riscv_vssubu_vx_u8m2(vuint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8m2(op1,op2,vl);
}


vuint8m4_t test___riscv_vssubu_vx_u8m4(vuint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8m4(op1,op2,vl);
}


vuint8m8_t test___riscv_vssubu_vx_u8m8(vuint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8m8(op1,op2,vl);
}


vuint16mf4_t test___riscv_vssubu_vx_u16mf4(vuint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16mf4(op1,op2,vl);
}


vuint16mf2_t test___riscv_vssubu_vx_u16mf2(vuint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16mf2(op1,op2,vl);
}


vuint16m1_t test___riscv_vssubu_vx_u16m1(vuint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16m1(op1,op2,vl);
}


vuint16m2_t test___riscv_vssubu_vx_u16m2(vuint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16m2(op1,op2,vl);
}


vuint16m4_t test___riscv_vssubu_vx_u16m4(vuint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16m4(op1,op2,vl);
}


vuint16m8_t test___riscv_vssubu_vx_u16m8(vuint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16m8(op1,op2,vl);
}


vuint32mf2_t test___riscv_vssubu_vx_u32mf2(vuint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u32mf2(op1,op2,vl);
}


vuint32m1_t test___riscv_vssubu_vx_u32m1(vuint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u32m1(op1,op2,vl);
}


vuint32m2_t test___riscv_vssubu_vx_u32m2(vuint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u32m2(op1,op2,vl);
}


vuint32m4_t test___riscv_vssubu_vx_u32m4(vuint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u32m4(op1,op2,vl);
}


vuint32m8_t test___riscv_vssubu_vx_u32m8(vuint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u32m8(op1,op2,vl);
}


vuint64m1_t test___riscv_vssubu_vx_u64m1(vuint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u64m1(op1,op2,vl);
}


vuint64m2_t test___riscv_vssubu_vx_u64m2(vuint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u64m2(op1,op2,vl);
}


vuint64m4_t test___riscv_vssubu_vx_u64m4(vuint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u64m4(op1,op2,vl);
}


vuint64m8_t test___riscv_vssubu_vx_u64m8(vuint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u64m8(op1,op2,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vssubu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
