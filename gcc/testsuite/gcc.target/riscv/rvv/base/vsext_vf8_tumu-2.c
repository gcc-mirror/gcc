/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint64m1_t test___riscv_vsext_vf8_i64m1_tumu(vbool64_t mask,vint64m1_t merge,vint8mf8_t op1,size_t vl)
{
    return __riscv_vsext_vf8_i64m1_tumu(mask,merge,op1,31);
}


vint64m2_t test___riscv_vsext_vf8_i64m2_tumu(vbool32_t mask,vint64m2_t merge,vint8mf4_t op1,size_t vl)
{
    return __riscv_vsext_vf8_i64m2_tumu(mask,merge,op1,31);
}


vint64m4_t test___riscv_vsext_vf8_i64m4_tumu(vbool16_t mask,vint64m4_t merge,vint8mf2_t op1,size_t vl)
{
    return __riscv_vsext_vf8_i64m4_tumu(mask,merge,op1,31);
}


vint64m8_t test___riscv_vsext_vf8_i64m8_tumu(vbool8_t mask,vint64m8_t merge,vint8m1_t op1,size_t vl)
{
    return __riscv_vsext_vf8_i64m8_tumu(mask,merge,op1,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*tu,\s*mu\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*tu,\s*mu\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*tu,\s*mu\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*tu,\s*mu\s+vsext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
