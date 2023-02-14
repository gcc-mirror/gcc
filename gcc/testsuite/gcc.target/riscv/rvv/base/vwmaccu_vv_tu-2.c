/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint16mf4_t test___riscv_vwmaccu_vv_u16mf4_tu(vuint16mf4_t vd,vuint8mf8_t vs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16mf4_tu(vd,vs1,vs2,31);
}


vuint16mf2_t test___riscv_vwmaccu_vv_u16mf2_tu(vuint16mf2_t vd,vuint8mf4_t vs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16mf2_tu(vd,vs1,vs2,31);
}


vuint16m1_t test___riscv_vwmaccu_vv_u16m1_tu(vuint16m1_t vd,vuint8mf2_t vs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16m1_tu(vd,vs1,vs2,31);
}


vuint16m2_t test___riscv_vwmaccu_vv_u16m2_tu(vuint16m2_t vd,vuint8m1_t vs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16m2_tu(vd,vs1,vs2,31);
}


vuint16m4_t test___riscv_vwmaccu_vv_u16m4_tu(vuint16m4_t vd,vuint8m2_t vs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16m4_tu(vd,vs1,vs2,31);
}


vuint16m8_t test___riscv_vwmaccu_vv_u16m8_tu(vuint16m8_t vd,vuint8m4_t vs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u16m8_tu(vd,vs1,vs2,31);
}


vuint32mf2_t test___riscv_vwmaccu_vv_u32mf2_tu(vuint32mf2_t vd,vuint16mf4_t vs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u32mf2_tu(vd,vs1,vs2,31);
}


vuint32m1_t test___riscv_vwmaccu_vv_u32m1_tu(vuint32m1_t vd,vuint16mf2_t vs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u32m1_tu(vd,vs1,vs2,31);
}


vuint32m2_t test___riscv_vwmaccu_vv_u32m2_tu(vuint32m2_t vd,vuint16m1_t vs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u32m2_tu(vd,vs1,vs2,31);
}


vuint32m4_t test___riscv_vwmaccu_vv_u32m4_tu(vuint32m4_t vd,vuint16m2_t vs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u32m4_tu(vd,vs1,vs2,31);
}


vuint32m8_t test___riscv_vwmaccu_vv_u32m8_tu(vuint32m8_t vd,vuint16m4_t vs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u32m8_tu(vd,vs1,vs2,31);
}


vuint64m1_t test___riscv_vwmaccu_vv_u64m1_tu(vuint64m1_t vd,vuint32mf2_t vs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u64m1_tu(vd,vs1,vs2,31);
}


vuint64m2_t test___riscv_vwmaccu_vv_u64m2_tu(vuint64m2_t vd,vuint32m1_t vs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u64m2_tu(vd,vs1,vs2,31);
}


vuint64m4_t test___riscv_vwmaccu_vv_u64m4_tu(vuint64m4_t vd,vuint32m2_t vs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u64m4_tu(vd,vs1,vs2,31);
}


vuint64m8_t test___riscv_vwmaccu_vv_u64m8_tu(vuint64m8_t vd,vuint32m4_t vs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vwmaccu_vv_u64m8_tu(vd,vs1,vs2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vwmaccu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
