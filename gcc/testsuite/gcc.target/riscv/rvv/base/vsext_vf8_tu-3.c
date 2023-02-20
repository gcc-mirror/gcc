/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint64m1_t test___riscv_vsext_vf8_i64m1_tu(vint64m1_t merge,vint8mf8_t op1,size_t vl)
{
    return __riscv_vsext_vf8_i64m1_tu(merge,op1,32);
}


vint64m2_t test___riscv_vsext_vf8_i64m2_tu(vint64m2_t merge,vint8mf4_t op1,size_t vl)
{
    return __riscv_vsext_vf8_i64m2_tu(merge,op1,32);
}


vint64m4_t test___riscv_vsext_vf8_i64m4_tu(vint64m4_t merge,vint8mf2_t op1,size_t vl)
{
    return __riscv_vsext_vf8_i64m4_tu(merge,op1,32);
}


vint64m8_t test___riscv_vsext_vf8_i64m8_tu(vint64m8_t merge,vint8m1_t op1,size_t vl)
{
    return __riscv_vsext_vf8_i64m8_tu(merge,op1,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*tu,\s*m[au]\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*tu,\s*m[au]\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*tu,\s*m[au]\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*tu,\s*m[au]\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+} 1 } } */
