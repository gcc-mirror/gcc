/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint64m1_t test___riscv_vzext_vf8_u64m1(vuint8mf8_t op1,size_t vl)
{
    return __riscv_vzext_vf8_u64m1(op1,31);
}


vuint64m2_t test___riscv_vzext_vf8_u64m2(vuint8mf4_t op1,size_t vl)
{
    return __riscv_vzext_vf8_u64m2(op1,31);
}


vuint64m4_t test___riscv_vzext_vf8_u64m4(vuint8mf2_t op1,size_t vl)
{
    return __riscv_vzext_vf8_u64m4(op1,31);
}


vuint64m8_t test___riscv_vzext_vf8_u64m8(vuint8m1_t op1,size_t vl)
{
    return __riscv_vzext_vf8_u64m8(op1,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+} 1 } } */
