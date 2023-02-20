/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmaccus(vint16mf4_t vd,uint8_t rs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint16mf2_t test___riscv_vwmaccus(vint16mf2_t vd,uint8_t rs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint16m1_t test___riscv_vwmaccus(vint16m1_t vd,uint8_t rs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint16m2_t test___riscv_vwmaccus(vint16m2_t vd,uint8_t rs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint16m4_t test___riscv_vwmaccus(vint16m4_t vd,uint8_t rs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint16m8_t test___riscv_vwmaccus(vint16m8_t vd,uint8_t rs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint32mf2_t test___riscv_vwmaccus(vint32mf2_t vd,uint16_t rs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint32m1_t test___riscv_vwmaccus(vint32m1_t vd,uint16_t rs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint32m2_t test___riscv_vwmaccus(vint32m2_t vd,uint16_t rs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint32m4_t test___riscv_vwmaccus(vint32m4_t vd,uint16_t rs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint32m8_t test___riscv_vwmaccus(vint32m8_t vd,uint16_t rs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint64m1_t test___riscv_vwmaccus(vint64m1_t vd,uint32_t rs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint64m2_t test___riscv_vwmaccus(vint64m2_t vd,uint32_t rs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint64m4_t test___riscv_vwmaccus(vint64m4_t vd,uint32_t rs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint64m8_t test___riscv_vwmaccus(vint64m8_t vd,uint32_t rs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus(vd,rs1,vs2,vl);
}


vint16mf4_t test___riscv_vwmaccus(vbool64_t mask,vint16mf4_t vd,uint8_t rs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint16mf2_t test___riscv_vwmaccus(vbool32_t mask,vint16mf2_t vd,uint8_t rs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint16m1_t test___riscv_vwmaccus(vbool16_t mask,vint16m1_t vd,uint8_t rs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint16m2_t test___riscv_vwmaccus(vbool8_t mask,vint16m2_t vd,uint8_t rs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint16m4_t test___riscv_vwmaccus(vbool4_t mask,vint16m4_t vd,uint8_t rs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint16m8_t test___riscv_vwmaccus(vbool2_t mask,vint16m8_t vd,uint8_t rs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint32mf2_t test___riscv_vwmaccus(vbool64_t mask,vint32mf2_t vd,uint16_t rs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint32m1_t test___riscv_vwmaccus(vbool32_t mask,vint32m1_t vd,uint16_t rs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint32m2_t test___riscv_vwmaccus(vbool16_t mask,vint32m2_t vd,uint16_t rs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint32m4_t test___riscv_vwmaccus(vbool8_t mask,vint32m4_t vd,uint16_t rs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint32m8_t test___riscv_vwmaccus(vbool4_t mask,vint32m8_t vd,uint16_t rs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint64m1_t test___riscv_vwmaccus(vbool64_t mask,vint64m1_t vd,uint32_t rs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint64m2_t test___riscv_vwmaccus(vbool32_t mask,vint64m2_t vd,uint32_t rs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint64m4_t test___riscv_vwmaccus(vbool16_t mask,vint64m4_t vd,uint32_t rs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}


vint64m8_t test___riscv_vwmaccus(vbool8_t mask,vint64m8_t vd,uint32_t rs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vwmaccus(mask,vd,rs1,vs2,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwmaccus\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
