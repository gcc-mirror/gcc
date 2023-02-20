/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint32mf2_t test___riscv_vzext_vf4_u32mf2_tu(vuint32mf2_t merge,vuint8mf8_t op1,size_t vl)
{
    return __riscv_vzext_vf4_u32mf2_tu(merge,op1,31);
}


vuint32m1_t test___riscv_vzext_vf4_u32m1_tu(vuint32m1_t merge,vuint8mf4_t op1,size_t vl)
{
    return __riscv_vzext_vf4_u32m1_tu(merge,op1,31);
}


vuint32m2_t test___riscv_vzext_vf4_u32m2_tu(vuint32m2_t merge,vuint8mf2_t op1,size_t vl)
{
    return __riscv_vzext_vf4_u32m2_tu(merge,op1,31);
}


vuint32m4_t test___riscv_vzext_vf4_u32m4_tu(vuint32m4_t merge,vuint8m1_t op1,size_t vl)
{
    return __riscv_vzext_vf4_u32m4_tu(merge,op1,31);
}


vuint32m8_t test___riscv_vzext_vf4_u32m8_tu(vuint32m8_t merge,vuint8m2_t op1,size_t vl)
{
    return __riscv_vzext_vf4_u32m8_tu(merge,op1,31);
}


vuint64m1_t test___riscv_vzext_vf4_u64m1_tu(vuint64m1_t merge,vuint16mf4_t op1,size_t vl)
{
    return __riscv_vzext_vf4_u64m1_tu(merge,op1,31);
}


vuint64m2_t test___riscv_vzext_vf4_u64m2_tu(vuint64m2_t merge,vuint16mf2_t op1,size_t vl)
{
    return __riscv_vzext_vf4_u64m2_tu(merge,op1,31);
}


vuint64m4_t test___riscv_vzext_vf4_u64m4_tu(vuint64m4_t merge,vuint16m1_t op1,size_t vl)
{
    return __riscv_vzext_vf4_u64m4_tu(merge,op1,31);
}


vuint64m8_t test___riscv_vzext_vf4_u64m8_tu(vuint64m8_t merge,vuint16m2_t op1,size_t vl)
{
    return __riscv_vzext_vf4_u64m8_tu(merge,op1,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vzext\.vf4\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vzext\.vf4\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vzext\.vf4\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vzext\.vf4\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vzext\.vf4\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*tu,\s*m[au]\s+vzext\.vf4\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*tu,\s*m[au]\s+vzext\.vf4\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*tu,\s*m[au]\s+vzext\.vf4\s+v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*tu,\s*m[au]\s+vzext\.vf4\s+v[0-9]+,\s*v[0-9]+} 1 } } */
