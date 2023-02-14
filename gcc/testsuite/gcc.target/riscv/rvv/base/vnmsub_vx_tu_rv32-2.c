/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vnmsub_vx_i8mf8_tu(vint8mf8_t vd,int8_t rs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i8mf8_tu(vd,rs1,vs2,31);
}


vint8mf4_t test___riscv_vnmsub_vx_i8mf4_tu(vint8mf4_t vd,int8_t rs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i8mf4_tu(vd,rs1,vs2,31);
}


vint8mf2_t test___riscv_vnmsub_vx_i8mf2_tu(vint8mf2_t vd,int8_t rs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i8mf2_tu(vd,rs1,vs2,31);
}


vint8m1_t test___riscv_vnmsub_vx_i8m1_tu(vint8m1_t vd,int8_t rs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i8m1_tu(vd,rs1,vs2,31);
}


vint8m2_t test___riscv_vnmsub_vx_i8m2_tu(vint8m2_t vd,int8_t rs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i8m2_tu(vd,rs1,vs2,31);
}


vint8m4_t test___riscv_vnmsub_vx_i8m4_tu(vint8m4_t vd,int8_t rs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i8m4_tu(vd,rs1,vs2,31);
}


vint8m8_t test___riscv_vnmsub_vx_i8m8_tu(vint8m8_t vd,int8_t rs1,vint8m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i8m8_tu(vd,rs1,vs2,31);
}


vint16mf4_t test___riscv_vnmsub_vx_i16mf4_tu(vint16mf4_t vd,int16_t rs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i16mf4_tu(vd,rs1,vs2,31);
}


vint16mf2_t test___riscv_vnmsub_vx_i16mf2_tu(vint16mf2_t vd,int16_t rs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i16mf2_tu(vd,rs1,vs2,31);
}


vint16m1_t test___riscv_vnmsub_vx_i16m1_tu(vint16m1_t vd,int16_t rs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i16m1_tu(vd,rs1,vs2,31);
}


vint16m2_t test___riscv_vnmsub_vx_i16m2_tu(vint16m2_t vd,int16_t rs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i16m2_tu(vd,rs1,vs2,31);
}


vint16m4_t test___riscv_vnmsub_vx_i16m4_tu(vint16m4_t vd,int16_t rs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i16m4_tu(vd,rs1,vs2,31);
}


vint16m8_t test___riscv_vnmsub_vx_i16m8_tu(vint16m8_t vd,int16_t rs1,vint16m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i16m8_tu(vd,rs1,vs2,31);
}


vint32mf2_t test___riscv_vnmsub_vx_i32mf2_tu(vint32mf2_t vd,int32_t rs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i32mf2_tu(vd,rs1,vs2,31);
}


vint32m1_t test___riscv_vnmsub_vx_i32m1_tu(vint32m1_t vd,int32_t rs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i32m1_tu(vd,rs1,vs2,31);
}


vint32m2_t test___riscv_vnmsub_vx_i32m2_tu(vint32m2_t vd,int32_t rs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i32m2_tu(vd,rs1,vs2,31);
}


vint32m4_t test___riscv_vnmsub_vx_i32m4_tu(vint32m4_t vd,int32_t rs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i32m4_tu(vd,rs1,vs2,31);
}


vint32m8_t test___riscv_vnmsub_vx_i32m8_tu(vint32m8_t vd,int32_t rs1,vint32m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i32m8_tu(vd,rs1,vs2,31);
}


vint64m1_t test___riscv_vnmsub_vx_i64m1_tu(vint64m1_t vd,int64_t rs1,vint64m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i64m1_tu(vd,rs1,vs2,31);
}


vint64m2_t test___riscv_vnmsub_vx_i64m2_tu(vint64m2_t vd,int64_t rs1,vint64m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i64m2_tu(vd,rs1,vs2,31);
}


vint64m4_t test___riscv_vnmsub_vx_i64m4_tu(vint64m4_t vd,int64_t rs1,vint64m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i64m4_tu(vd,rs1,vs2,31);
}


vint64m8_t test___riscv_vnmsub_vx_i64m8_tu(vint64m8_t vd,int64_t rs1,vint64m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_i64m8_tu(vd,rs1,vs2,31);
}


vuint8mf8_t test___riscv_vnmsub_vx_u8mf8_tu(vuint8mf8_t vd,uint8_t rs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u8mf8_tu(vd,rs1,vs2,31);
}


vuint8mf4_t test___riscv_vnmsub_vx_u8mf4_tu(vuint8mf4_t vd,uint8_t rs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u8mf4_tu(vd,rs1,vs2,31);
}


vuint8mf2_t test___riscv_vnmsub_vx_u8mf2_tu(vuint8mf2_t vd,uint8_t rs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u8mf2_tu(vd,rs1,vs2,31);
}


vuint8m1_t test___riscv_vnmsub_vx_u8m1_tu(vuint8m1_t vd,uint8_t rs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u8m1_tu(vd,rs1,vs2,31);
}


vuint8m2_t test___riscv_vnmsub_vx_u8m2_tu(vuint8m2_t vd,uint8_t rs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u8m2_tu(vd,rs1,vs2,31);
}


vuint8m4_t test___riscv_vnmsub_vx_u8m4_tu(vuint8m4_t vd,uint8_t rs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u8m4_tu(vd,rs1,vs2,31);
}


vuint8m8_t test___riscv_vnmsub_vx_u8m8_tu(vuint8m8_t vd,uint8_t rs1,vuint8m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u8m8_tu(vd,rs1,vs2,31);
}


vuint16mf4_t test___riscv_vnmsub_vx_u16mf4_tu(vuint16mf4_t vd,uint16_t rs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u16mf4_tu(vd,rs1,vs2,31);
}


vuint16mf2_t test___riscv_vnmsub_vx_u16mf2_tu(vuint16mf2_t vd,uint16_t rs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u16mf2_tu(vd,rs1,vs2,31);
}


vuint16m1_t test___riscv_vnmsub_vx_u16m1_tu(vuint16m1_t vd,uint16_t rs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u16m1_tu(vd,rs1,vs2,31);
}


vuint16m2_t test___riscv_vnmsub_vx_u16m2_tu(vuint16m2_t vd,uint16_t rs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u16m2_tu(vd,rs1,vs2,31);
}


vuint16m4_t test___riscv_vnmsub_vx_u16m4_tu(vuint16m4_t vd,uint16_t rs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u16m4_tu(vd,rs1,vs2,31);
}


vuint16m8_t test___riscv_vnmsub_vx_u16m8_tu(vuint16m8_t vd,uint16_t rs1,vuint16m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u16m8_tu(vd,rs1,vs2,31);
}


vuint32mf2_t test___riscv_vnmsub_vx_u32mf2_tu(vuint32mf2_t vd,uint32_t rs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u32mf2_tu(vd,rs1,vs2,31);
}


vuint32m1_t test___riscv_vnmsub_vx_u32m1_tu(vuint32m1_t vd,uint32_t rs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u32m1_tu(vd,rs1,vs2,31);
}


vuint32m2_t test___riscv_vnmsub_vx_u32m2_tu(vuint32m2_t vd,uint32_t rs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u32m2_tu(vd,rs1,vs2,31);
}


vuint32m4_t test___riscv_vnmsub_vx_u32m4_tu(vuint32m4_t vd,uint32_t rs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u32m4_tu(vd,rs1,vs2,31);
}


vuint32m8_t test___riscv_vnmsub_vx_u32m8_tu(vuint32m8_t vd,uint32_t rs1,vuint32m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u32m8_tu(vd,rs1,vs2,31);
}


vuint64m1_t test___riscv_vnmsub_vx_u64m1_tu(vuint64m1_t vd,uint64_t rs1,vuint64m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u64m1_tu(vd,rs1,vs2,31);
}


vuint64m2_t test___riscv_vnmsub_vx_u64m2_tu(vuint64m2_t vd,uint64_t rs1,vuint64m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u64m2_tu(vd,rs1,vs2,31);
}


vuint64m4_t test___riscv_vnmsub_vx_u64m4_tu(vuint64m4_t vd,uint64_t rs1,vuint64m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u64m4_tu(vd,rs1,vs2,31);
}


vuint64m8_t test___riscv_vnmsub_vx_u64m8_tu(vuint64m8_t vd,uint64_t rs1,vuint64m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_vx_u64m8_tu(vd,rs1,vs2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vnms[a-u][b-c]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 8 } } */
