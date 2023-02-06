/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint32mf2_t test___riscv_vsext_vf4_i32mf2_m(vbool64_t mask,vint8mf8_t op1,size_t vl)
{
    return __riscv_vsext_vf4_i32mf2_m(mask,op1,32);
}


vint32m1_t test___riscv_vsext_vf4_i32m1_m(vbool32_t mask,vint8mf4_t op1,size_t vl)
{
    return __riscv_vsext_vf4_i32m1_m(mask,op1,32);
}


vint32m2_t test___riscv_vsext_vf4_i32m2_m(vbool16_t mask,vint8mf2_t op1,size_t vl)
{
    return __riscv_vsext_vf4_i32m2_m(mask,op1,32);
}


vint32m4_t test___riscv_vsext_vf4_i32m4_m(vbool8_t mask,vint8m1_t op1,size_t vl)
{
    return __riscv_vsext_vf4_i32m4_m(mask,op1,32);
}


vint32m8_t test___riscv_vsext_vf4_i32m8_m(vbool4_t mask,vint8m2_t op1,size_t vl)
{
    return __riscv_vsext_vf4_i32m8_m(mask,op1,32);
}


vint64m1_t test___riscv_vsext_vf4_i64m1_m(vbool64_t mask,vint16mf4_t op1,size_t vl)
{
    return __riscv_vsext_vf4_i64m1_m(mask,op1,32);
}


vint64m2_t test___riscv_vsext_vf4_i64m2_m(vbool32_t mask,vint16mf2_t op1,size_t vl)
{
    return __riscv_vsext_vf4_i64m2_m(mask,op1,32);
}


vint64m4_t test___riscv_vsext_vf4_i64m4_m(vbool16_t mask,vint16m1_t op1,size_t vl)
{
    return __riscv_vsext_vf4_i64m4_m(mask,op1,32);
}


vint64m8_t test___riscv_vsext_vf4_i64m8_m(vbool8_t mask,vint16m2_t op1,size_t vl)
{
    return __riscv_vsext_vf4_i64m8_m(mask,op1,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
