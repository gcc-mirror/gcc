/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmaccus_vx_i16mf4_tum(vbool64_t mask,vint16mf4_t vd,uint8_t rs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i16mf4_tum(mask,vd,rs1,vs2,31);
}


vint16mf2_t test___riscv_vwmaccus_vx_i16mf2_tum(vbool32_t mask,vint16mf2_t vd,uint8_t rs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i16mf2_tum(mask,vd,rs1,vs2,31);
}


vint16m1_t test___riscv_vwmaccus_vx_i16m1_tum(vbool16_t mask,vint16m1_t vd,uint8_t rs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i16m1_tum(mask,vd,rs1,vs2,31);
}


vint16m2_t test___riscv_vwmaccus_vx_i16m2_tum(vbool8_t mask,vint16m2_t vd,uint8_t rs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i16m2_tum(mask,vd,rs1,vs2,31);
}


vint16m4_t test___riscv_vwmaccus_vx_i16m4_tum(vbool4_t mask,vint16m4_t vd,uint8_t rs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i16m4_tum(mask,vd,rs1,vs2,31);
}


vint16m8_t test___riscv_vwmaccus_vx_i16m8_tum(vbool2_t mask,vint16m8_t vd,uint8_t rs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i16m8_tum(mask,vd,rs1,vs2,31);
}


vint32mf2_t test___riscv_vwmaccus_vx_i32mf2_tum(vbool64_t mask,vint32mf2_t vd,uint16_t rs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i32mf2_tum(mask,vd,rs1,vs2,31);
}


vint32m1_t test___riscv_vwmaccus_vx_i32m1_tum(vbool32_t mask,vint32m1_t vd,uint16_t rs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i32m1_tum(mask,vd,rs1,vs2,31);
}


vint32m2_t test___riscv_vwmaccus_vx_i32m2_tum(vbool16_t mask,vint32m2_t vd,uint16_t rs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i32m2_tum(mask,vd,rs1,vs2,31);
}


vint32m4_t test___riscv_vwmaccus_vx_i32m4_tum(vbool8_t mask,vint32m4_t vd,uint16_t rs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i32m4_tum(mask,vd,rs1,vs2,31);
}


vint32m8_t test___riscv_vwmaccus_vx_i32m8_tum(vbool4_t mask,vint32m8_t vd,uint16_t rs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i32m8_tum(mask,vd,rs1,vs2,31);
}


vint64m1_t test___riscv_vwmaccus_vx_i64m1_tum(vbool64_t mask,vint64m1_t vd,uint32_t rs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i64m1_tum(mask,vd,rs1,vs2,31);
}


vint64m2_t test___riscv_vwmaccus_vx_i64m2_tum(vbool32_t mask,vint64m2_t vd,uint32_t rs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i64m2_tum(mask,vd,rs1,vs2,31);
}


vint64m4_t test___riscv_vwmaccus_vx_i64m4_tum(vbool16_t mask,vint64m4_t vd,uint32_t rs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i64m4_tum(mask,vd,rs1,vs2,31);
}


vint64m8_t test___riscv_vwmaccus_vx_i64m8_tum(vbool8_t mask,vint64m8_t vd,uint32_t rs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus_vx_i64m8_tum(mask,vd,rs1,vs2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
