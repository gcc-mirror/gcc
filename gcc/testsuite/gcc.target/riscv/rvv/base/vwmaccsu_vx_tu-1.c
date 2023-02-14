/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmaccsu_vx_i16mf4_tu(vint16mf4_t vd,int8_t rs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16mf4_tu(vd,rs1,vs2,vl);
}


vint16mf2_t test___riscv_vwmaccsu_vx_i16mf2_tu(vint16mf2_t vd,int8_t rs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16mf2_tu(vd,rs1,vs2,vl);
}


vint16m1_t test___riscv_vwmaccsu_vx_i16m1_tu(vint16m1_t vd,int8_t rs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16m1_tu(vd,rs1,vs2,vl);
}


vint16m2_t test___riscv_vwmaccsu_vx_i16m2_tu(vint16m2_t vd,int8_t rs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16m2_tu(vd,rs1,vs2,vl);
}


vint16m4_t test___riscv_vwmaccsu_vx_i16m4_tu(vint16m4_t vd,int8_t rs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16m4_tu(vd,rs1,vs2,vl);
}


vint16m8_t test___riscv_vwmaccsu_vx_i16m8_tu(vint16m8_t vd,int8_t rs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i16m8_tu(vd,rs1,vs2,vl);
}


vint32mf2_t test___riscv_vwmaccsu_vx_i32mf2_tu(vint32mf2_t vd,int16_t rs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i32mf2_tu(vd,rs1,vs2,vl);
}


vint32m1_t test___riscv_vwmaccsu_vx_i32m1_tu(vint32m1_t vd,int16_t rs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i32m1_tu(vd,rs1,vs2,vl);
}


vint32m2_t test___riscv_vwmaccsu_vx_i32m2_tu(vint32m2_t vd,int16_t rs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i32m2_tu(vd,rs1,vs2,vl);
}


vint32m4_t test___riscv_vwmaccsu_vx_i32m4_tu(vint32m4_t vd,int16_t rs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i32m4_tu(vd,rs1,vs2,vl);
}


vint32m8_t test___riscv_vwmaccsu_vx_i32m8_tu(vint32m8_t vd,int16_t rs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i32m8_tu(vd,rs1,vs2,vl);
}


vint64m1_t test___riscv_vwmaccsu_vx_i64m1_tu(vint64m1_t vd,int32_t rs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i64m1_tu(vd,rs1,vs2,vl);
}


vint64m2_t test___riscv_vwmaccsu_vx_i64m2_tu(vint64m2_t vd,int32_t rs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i64m2_tu(vd,rs1,vs2,vl);
}


vint64m4_t test___riscv_vwmaccsu_vx_i64m4_tu(vint64m4_t vd,int32_t rs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i64m4_tu(vd,rs1,vs2,vl);
}


vint64m8_t test___riscv_vwmaccsu_vx_i64m8_tu(vint64m8_t vd,int32_t rs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vwmaccsu_vx_i64m8_tu(vd,rs1,vs2,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vwmaccsu\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+} 1 } } */
