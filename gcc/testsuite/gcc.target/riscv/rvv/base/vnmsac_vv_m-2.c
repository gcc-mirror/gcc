/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vnmsac_vv_i8mf8_m(vbool64_t mask,vint8mf8_t vd,vint8mf8_t vs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i8mf8_m(mask,vd,vs1,vs2,31);
}


vint8mf4_t test___riscv_vnmsac_vv_i8mf4_m(vbool32_t mask,vint8mf4_t vd,vint8mf4_t vs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i8mf4_m(mask,vd,vs1,vs2,31);
}


vint8mf2_t test___riscv_vnmsac_vv_i8mf2_m(vbool16_t mask,vint8mf2_t vd,vint8mf2_t vs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i8mf2_m(mask,vd,vs1,vs2,31);
}


vint8m1_t test___riscv_vnmsac_vv_i8m1_m(vbool8_t mask,vint8m1_t vd,vint8m1_t vs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i8m1_m(mask,vd,vs1,vs2,31);
}


vint8m2_t test___riscv_vnmsac_vv_i8m2_m(vbool4_t mask,vint8m2_t vd,vint8m2_t vs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i8m2_m(mask,vd,vs1,vs2,31);
}


vint8m4_t test___riscv_vnmsac_vv_i8m4_m(vbool2_t mask,vint8m4_t vd,vint8m4_t vs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i8m4_m(mask,vd,vs1,vs2,31);
}


vint8m8_t test___riscv_vnmsac_vv_i8m8_m(vbool1_t mask,vint8m8_t vd,vint8m8_t vs1,vint8m8_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i8m8_m(mask,vd,vs1,vs2,31);
}


vint16mf4_t test___riscv_vnmsac_vv_i16mf4_m(vbool64_t mask,vint16mf4_t vd,vint16mf4_t vs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i16mf4_m(mask,vd,vs1,vs2,31);
}


vint16mf2_t test___riscv_vnmsac_vv_i16mf2_m(vbool32_t mask,vint16mf2_t vd,vint16mf2_t vs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i16mf2_m(mask,vd,vs1,vs2,31);
}


vint16m1_t test___riscv_vnmsac_vv_i16m1_m(vbool16_t mask,vint16m1_t vd,vint16m1_t vs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i16m1_m(mask,vd,vs1,vs2,31);
}


vint16m2_t test___riscv_vnmsac_vv_i16m2_m(vbool8_t mask,vint16m2_t vd,vint16m2_t vs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i16m2_m(mask,vd,vs1,vs2,31);
}


vint16m4_t test___riscv_vnmsac_vv_i16m4_m(vbool4_t mask,vint16m4_t vd,vint16m4_t vs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i16m4_m(mask,vd,vs1,vs2,31);
}


vint16m8_t test___riscv_vnmsac_vv_i16m8_m(vbool2_t mask,vint16m8_t vd,vint16m8_t vs1,vint16m8_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i16m8_m(mask,vd,vs1,vs2,31);
}


vint32mf2_t test___riscv_vnmsac_vv_i32mf2_m(vbool64_t mask,vint32mf2_t vd,vint32mf2_t vs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i32mf2_m(mask,vd,vs1,vs2,31);
}


vint32m1_t test___riscv_vnmsac_vv_i32m1_m(vbool32_t mask,vint32m1_t vd,vint32m1_t vs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i32m1_m(mask,vd,vs1,vs2,31);
}


vint32m2_t test___riscv_vnmsac_vv_i32m2_m(vbool16_t mask,vint32m2_t vd,vint32m2_t vs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i32m2_m(mask,vd,vs1,vs2,31);
}


vint32m4_t test___riscv_vnmsac_vv_i32m4_m(vbool8_t mask,vint32m4_t vd,vint32m4_t vs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i32m4_m(mask,vd,vs1,vs2,31);
}


vint32m8_t test___riscv_vnmsac_vv_i32m8_m(vbool4_t mask,vint32m8_t vd,vint32m8_t vs1,vint32m8_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i32m8_m(mask,vd,vs1,vs2,31);
}


vint64m1_t test___riscv_vnmsac_vv_i64m1_m(vbool64_t mask,vint64m1_t vd,vint64m1_t vs1,vint64m1_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i64m1_m(mask,vd,vs1,vs2,31);
}


vint64m2_t test___riscv_vnmsac_vv_i64m2_m(vbool32_t mask,vint64m2_t vd,vint64m2_t vs1,vint64m2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i64m2_m(mask,vd,vs1,vs2,31);
}


vint64m4_t test___riscv_vnmsac_vv_i64m4_m(vbool16_t mask,vint64m4_t vd,vint64m4_t vs1,vint64m4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i64m4_m(mask,vd,vs1,vs2,31);
}


vint64m8_t test___riscv_vnmsac_vv_i64m8_m(vbool8_t mask,vint64m8_t vd,vint64m8_t vs1,vint64m8_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_i64m8_m(mask,vd,vs1,vs2,31);
}


vuint8mf8_t test___riscv_vnmsac_vv_u8mf8_m(vbool64_t mask,vuint8mf8_t vd,vuint8mf8_t vs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u8mf8_m(mask,vd,vs1,vs2,31);
}


vuint8mf4_t test___riscv_vnmsac_vv_u8mf4_m(vbool32_t mask,vuint8mf4_t vd,vuint8mf4_t vs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u8mf4_m(mask,vd,vs1,vs2,31);
}


vuint8mf2_t test___riscv_vnmsac_vv_u8mf2_m(vbool16_t mask,vuint8mf2_t vd,vuint8mf2_t vs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u8mf2_m(mask,vd,vs1,vs2,31);
}


vuint8m1_t test___riscv_vnmsac_vv_u8m1_m(vbool8_t mask,vuint8m1_t vd,vuint8m1_t vs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u8m1_m(mask,vd,vs1,vs2,31);
}


vuint8m2_t test___riscv_vnmsac_vv_u8m2_m(vbool4_t mask,vuint8m2_t vd,vuint8m2_t vs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u8m2_m(mask,vd,vs1,vs2,31);
}


vuint8m4_t test___riscv_vnmsac_vv_u8m4_m(vbool2_t mask,vuint8m4_t vd,vuint8m4_t vs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u8m4_m(mask,vd,vs1,vs2,31);
}


vuint8m8_t test___riscv_vnmsac_vv_u8m8_m(vbool1_t mask,vuint8m8_t vd,vuint8m8_t vs1,vuint8m8_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u8m8_m(mask,vd,vs1,vs2,31);
}


vuint16mf4_t test___riscv_vnmsac_vv_u16mf4_m(vbool64_t mask,vuint16mf4_t vd,vuint16mf4_t vs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u16mf4_m(mask,vd,vs1,vs2,31);
}


vuint16mf2_t test___riscv_vnmsac_vv_u16mf2_m(vbool32_t mask,vuint16mf2_t vd,vuint16mf2_t vs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u16mf2_m(mask,vd,vs1,vs2,31);
}


vuint16m1_t test___riscv_vnmsac_vv_u16m1_m(vbool16_t mask,vuint16m1_t vd,vuint16m1_t vs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u16m1_m(mask,vd,vs1,vs2,31);
}


vuint16m2_t test___riscv_vnmsac_vv_u16m2_m(vbool8_t mask,vuint16m2_t vd,vuint16m2_t vs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u16m2_m(mask,vd,vs1,vs2,31);
}


vuint16m4_t test___riscv_vnmsac_vv_u16m4_m(vbool4_t mask,vuint16m4_t vd,vuint16m4_t vs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u16m4_m(mask,vd,vs1,vs2,31);
}


vuint16m8_t test___riscv_vnmsac_vv_u16m8_m(vbool2_t mask,vuint16m8_t vd,vuint16m8_t vs1,vuint16m8_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u16m8_m(mask,vd,vs1,vs2,31);
}


vuint32mf2_t test___riscv_vnmsac_vv_u32mf2_m(vbool64_t mask,vuint32mf2_t vd,vuint32mf2_t vs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u32mf2_m(mask,vd,vs1,vs2,31);
}


vuint32m1_t test___riscv_vnmsac_vv_u32m1_m(vbool32_t mask,vuint32m1_t vd,vuint32m1_t vs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u32m1_m(mask,vd,vs1,vs2,31);
}


vuint32m2_t test___riscv_vnmsac_vv_u32m2_m(vbool16_t mask,vuint32m2_t vd,vuint32m2_t vs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u32m2_m(mask,vd,vs1,vs2,31);
}


vuint32m4_t test___riscv_vnmsac_vv_u32m4_m(vbool8_t mask,vuint32m4_t vd,vuint32m4_t vs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u32m4_m(mask,vd,vs1,vs2,31);
}


vuint32m8_t test___riscv_vnmsac_vv_u32m8_m(vbool4_t mask,vuint32m8_t vd,vuint32m8_t vs1,vuint32m8_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u32m8_m(mask,vd,vs1,vs2,31);
}


vuint64m1_t test___riscv_vnmsac_vv_u64m1_m(vbool64_t mask,vuint64m1_t vd,vuint64m1_t vs1,vuint64m1_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u64m1_m(mask,vd,vs1,vs2,31);
}


vuint64m2_t test___riscv_vnmsac_vv_u64m2_m(vbool32_t mask,vuint64m2_t vd,vuint64m2_t vs1,vuint64m2_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u64m2_m(mask,vd,vs1,vs2,31);
}


vuint64m4_t test___riscv_vnmsac_vv_u64m4_m(vbool16_t mask,vuint64m4_t vd,vuint64m4_t vs1,vuint64m4_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u64m4_m(mask,vd,vs1,vs2,31);
}


vuint64m8_t test___riscv_vnmsac_vv_u64m8_m(vbool8_t mask,vuint64m8_t vd,vuint64m8_t vs1,vuint64m8_t vs2,size_t vl)
{
    return __riscv_vnmsac_vv_u64m8_m(mask,vd,vs1,vs2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
