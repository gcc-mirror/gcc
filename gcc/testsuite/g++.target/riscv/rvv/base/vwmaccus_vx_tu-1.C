/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmaccus_tu(vint16mf4_t vd,uint8_t rs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint16mf2_t test___riscv_vwmaccus_tu(vint16mf2_t vd,uint8_t rs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint16m1_t test___riscv_vwmaccus_tu(vint16m1_t vd,uint8_t rs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint16m2_t test___riscv_vwmaccus_tu(vint16m2_t vd,uint8_t rs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint16m4_t test___riscv_vwmaccus_tu(vint16m4_t vd,uint8_t rs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint16m8_t test___riscv_vwmaccus_tu(vint16m8_t vd,uint8_t rs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint32mf2_t test___riscv_vwmaccus_tu(vint32mf2_t vd,uint16_t rs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint32m1_t test___riscv_vwmaccus_tu(vint32m1_t vd,uint16_t rs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint32m2_t test___riscv_vwmaccus_tu(vint32m2_t vd,uint16_t rs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint32m4_t test___riscv_vwmaccus_tu(vint32m4_t vd,uint16_t rs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint32m8_t test___riscv_vwmaccus_tu(vint32m8_t vd,uint16_t rs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint64m1_t test___riscv_vwmaccus_tu(vint64m1_t vd,uint32_t rs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint64m2_t test___riscv_vwmaccus_tu(vint64m2_t vd,uint32_t rs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint64m4_t test___riscv_vwmaccus_tu(vint64m4_t vd,uint32_t rs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}


vint64m8_t test___riscv_vwmaccus_tu(vint64m8_t vd,uint32_t rs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus_tu(vd,rs1,vs2,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
