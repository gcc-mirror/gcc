/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint8mf8_t test___riscv_vaaddu_vv_u8mf8(vuint8mf8_t op1,vuint8mf8_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u8mf8(op1,op2,32);
}


vuint8mf4_t test___riscv_vaaddu_vv_u8mf4(vuint8mf4_t op1,vuint8mf4_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u8mf4(op1,op2,32);
}


vuint8mf2_t test___riscv_vaaddu_vv_u8mf2(vuint8mf2_t op1,vuint8mf2_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u8mf2(op1,op2,32);
}


vuint8m1_t test___riscv_vaaddu_vv_u8m1(vuint8m1_t op1,vuint8m1_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u8m1(op1,op2,32);
}


vuint8m2_t test___riscv_vaaddu_vv_u8m2(vuint8m2_t op1,vuint8m2_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u8m2(op1,op2,32);
}


vuint8m4_t test___riscv_vaaddu_vv_u8m4(vuint8m4_t op1,vuint8m4_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u8m4(op1,op2,32);
}


vuint8m8_t test___riscv_vaaddu_vv_u8m8(vuint8m8_t op1,vuint8m8_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u8m8(op1,op2,32);
}


vuint16mf4_t test___riscv_vaaddu_vv_u16mf4(vuint16mf4_t op1,vuint16mf4_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u16mf4(op1,op2,32);
}


vuint16mf2_t test___riscv_vaaddu_vv_u16mf2(vuint16mf2_t op1,vuint16mf2_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u16mf2(op1,op2,32);
}


vuint16m1_t test___riscv_vaaddu_vv_u16m1(vuint16m1_t op1,vuint16m1_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u16m1(op1,op2,32);
}


vuint16m2_t test___riscv_vaaddu_vv_u16m2(vuint16m2_t op1,vuint16m2_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u16m2(op1,op2,32);
}


vuint16m4_t test___riscv_vaaddu_vv_u16m4(vuint16m4_t op1,vuint16m4_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u16m4(op1,op2,32);
}


vuint16m8_t test___riscv_vaaddu_vv_u16m8(vuint16m8_t op1,vuint16m8_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u16m8(op1,op2,32);
}


vuint32mf2_t test___riscv_vaaddu_vv_u32mf2(vuint32mf2_t op1,vuint32mf2_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u32mf2(op1,op2,32);
}


vuint32m1_t test___riscv_vaaddu_vv_u32m1(vuint32m1_t op1,vuint32m1_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u32m1(op1,op2,32);
}


vuint32m2_t test___riscv_vaaddu_vv_u32m2(vuint32m2_t op1,vuint32m2_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u32m2(op1,op2,32);
}


vuint32m4_t test___riscv_vaaddu_vv_u32m4(vuint32m4_t op1,vuint32m4_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u32m4(op1,op2,32);
}


vuint32m8_t test___riscv_vaaddu_vv_u32m8(vuint32m8_t op1,vuint32m8_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u32m8(op1,op2,32);
}


vuint64m1_t test___riscv_vaaddu_vv_u64m1(vuint64m1_t op1,vuint64m1_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u64m1(op1,op2,32);
}


vuint64m2_t test___riscv_vaaddu_vv_u64m2(vuint64m2_t op1,vuint64m2_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u64m2(op1,op2,32);
}


vuint64m4_t test___riscv_vaaddu_vv_u64m4(vuint64m4_t op1,vuint64m4_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u64m4(op1,op2,32);
}


vuint64m8_t test___riscv_vaaddu_vv_u64m8(vuint64m8_t op1,vuint64m8_t op2,size_t vl)
{
    return __riscv_vaaddu_vv_u64m8(op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
