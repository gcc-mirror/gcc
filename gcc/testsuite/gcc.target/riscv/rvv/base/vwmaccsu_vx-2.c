/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmaccsu_vx_i16mf4(vint16mf4_t vd,int8_t rs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16mf4(vd,rs1,vs2,31);
}


vint16mf2_t test___riscv_vwmaccsu_vx_i16mf2(vint16mf2_t vd,int8_t rs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16mf2(vd,rs1,vs2,31);
}


vint16m1_t test___riscv_vwmaccsu_vx_i16m1(vint16m1_t vd,int8_t rs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16m1(vd,rs1,vs2,31);
}


vint16m2_t test___riscv_vwmaccsu_vx_i16m2(vint16m2_t vd,int8_t rs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16m2(vd,rs1,vs2,31);
}


vint16m4_t test___riscv_vwmaccsu_vx_i16m4(vint16m4_t vd,int8_t rs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16m4(vd,rs1,vs2,31);
}


vint16m8_t test___riscv_vwmaccsu_vx_i16m8(vint16m8_t vd,int8_t rs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16m8(vd,rs1,vs2,31);
}


vint32mf2_t test___riscv_vwmaccsu_vx_i32mf2(vint32mf2_t vd,int16_t rs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i32mf2(vd,rs1,vs2,31);
}


vint32m1_t test___riscv_vwmaccsu_vx_i32m1(vint32m1_t vd,int16_t rs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i32m1(vd,rs1,vs2,31);
}


vint32m2_t test___riscv_vwmaccsu_vx_i32m2(vint32m2_t vd,int16_t rs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i32m2(vd,rs1,vs2,31);
}


vint32m4_t test___riscv_vwmaccsu_vx_i32m4(vint32m4_t vd,int16_t rs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i32m4(vd,rs1,vs2,31);
}


vint32m8_t test___riscv_vwmaccsu_vx_i32m8(vint32m8_t vd,int16_t rs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i32m8(vd,rs1,vs2,31);
}


vint64m1_t test___riscv_vwmaccsu_vx_i64m1(vint64m1_t vd,int32_t rs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i64m1(vd,rs1,vs2,31);
}


vint64m2_t test___riscv_vwmaccsu_vx_i64m2(vint64m2_t vd,int32_t rs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i64m2(vd,rs1,vs2,31);
}


vint64m4_t test___riscv_vwmaccsu_vx_i64m4(vint64m4_t vd,int32_t rs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i64m4(vd,rs1,vs2,31);
}


vint64m8_t test___riscv_vwmaccsu_vx_i64m8(vint64m8_t vd,int32_t rs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i64m8(vd,rs1,vs2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
