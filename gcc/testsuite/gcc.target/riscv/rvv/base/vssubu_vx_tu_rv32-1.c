/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint8mf8_t test___riscv_vssubu_vx_u8mf8_tu(vuint8mf8_t merge,vuint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8mf8_tu(merge,op1,op2,vl);
}


vuint8mf4_t test___riscv_vssubu_vx_u8mf4_tu(vuint8mf4_t merge,vuint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8mf4_tu(merge,op1,op2,vl);
}


vuint8mf2_t test___riscv_vssubu_vx_u8mf2_tu(vuint8mf2_t merge,vuint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8mf2_tu(merge,op1,op2,vl);
}


vuint8m1_t test___riscv_vssubu_vx_u8m1_tu(vuint8m1_t merge,vuint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8m1_tu(merge,op1,op2,vl);
}


vuint8m2_t test___riscv_vssubu_vx_u8m2_tu(vuint8m2_t merge,vuint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8m2_tu(merge,op1,op2,vl);
}


vuint8m4_t test___riscv_vssubu_vx_u8m4_tu(vuint8m4_t merge,vuint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8m4_tu(merge,op1,op2,vl);
}


vuint8m8_t test___riscv_vssubu_vx_u8m8_tu(vuint8m8_t merge,vuint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u8m8_tu(merge,op1,op2,vl);
}


vuint16mf4_t test___riscv_vssubu_vx_u16mf4_tu(vuint16mf4_t merge,vuint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16mf4_tu(merge,op1,op2,vl);
}


vuint16mf2_t test___riscv_vssubu_vx_u16mf2_tu(vuint16mf2_t merge,vuint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16mf2_tu(merge,op1,op2,vl);
}


vuint16m1_t test___riscv_vssubu_vx_u16m1_tu(vuint16m1_t merge,vuint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16m1_tu(merge,op1,op2,vl);
}


vuint16m2_t test___riscv_vssubu_vx_u16m2_tu(vuint16m2_t merge,vuint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16m2_tu(merge,op1,op2,vl);
}


vuint16m4_t test___riscv_vssubu_vx_u16m4_tu(vuint16m4_t merge,vuint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16m4_tu(merge,op1,op2,vl);
}


vuint16m8_t test___riscv_vssubu_vx_u16m8_tu(vuint16m8_t merge,vuint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u16m8_tu(merge,op1,op2,vl);
}


vuint32mf2_t test___riscv_vssubu_vx_u32mf2_tu(vuint32mf2_t merge,vuint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u32mf2_tu(merge,op1,op2,vl);
}


vuint32m1_t test___riscv_vssubu_vx_u32m1_tu(vuint32m1_t merge,vuint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u32m1_tu(merge,op1,op2,vl);
}


vuint32m2_t test___riscv_vssubu_vx_u32m2_tu(vuint32m2_t merge,vuint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u32m2_tu(merge,op1,op2,vl);
}


vuint32m4_t test___riscv_vssubu_vx_u32m4_tu(vuint32m4_t merge,vuint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u32m4_tu(merge,op1,op2,vl);
}


vuint32m8_t test___riscv_vssubu_vx_u32m8_tu(vuint32m8_t merge,vuint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u32m8_tu(merge,op1,op2,vl);
}


vuint64m1_t test___riscv_vssubu_vx_u64m1_tu(vuint64m1_t merge,vuint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u64m1_tu(merge,op1,op2,vl);
}


vuint64m2_t test___riscv_vssubu_vx_u64m2_tu(vuint64m2_t merge,vuint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u64m2_tu(merge,op1,op2,vl);
}


vuint64m4_t test___riscv_vssubu_vx_u64m4_tu(vuint64m4_t merge,vuint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u64m4_tu(merge,op1,op2,vl);
}


vuint64m8_t test___riscv_vssubu_vx_u64m8_tu(vuint64m8_t merge,vuint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vssubu_vx_u64m8_tu(merge,op1,op2,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vssubu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vssubu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 4 } } */
