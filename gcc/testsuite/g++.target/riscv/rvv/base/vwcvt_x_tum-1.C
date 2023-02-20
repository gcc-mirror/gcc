/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwcvt_x_tum(vbool64_t mask,vint16mf4_t merge,vint8mf8_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint16mf2_t test___riscv_vwcvt_x_tum(vbool32_t mask,vint16mf2_t merge,vint8mf4_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint16m1_t test___riscv_vwcvt_x_tum(vbool16_t mask,vint16m1_t merge,vint8mf2_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint16m2_t test___riscv_vwcvt_x_tum(vbool8_t mask,vint16m2_t merge,vint8m1_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint16m4_t test___riscv_vwcvt_x_tum(vbool4_t mask,vint16m4_t merge,vint8m2_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint16m8_t test___riscv_vwcvt_x_tum(vbool2_t mask,vint16m8_t merge,vint8m4_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint32mf2_t test___riscv_vwcvt_x_tum(vbool64_t mask,vint32mf2_t merge,vint16mf4_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint32m1_t test___riscv_vwcvt_x_tum(vbool32_t mask,vint32m1_t merge,vint16mf2_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint32m2_t test___riscv_vwcvt_x_tum(vbool16_t mask,vint32m2_t merge,vint16m1_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint32m4_t test___riscv_vwcvt_x_tum(vbool8_t mask,vint32m4_t merge,vint16m2_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint32m8_t test___riscv_vwcvt_x_tum(vbool4_t mask,vint32m8_t merge,vint16m4_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint64m1_t test___riscv_vwcvt_x_tum(vbool64_t mask,vint64m1_t merge,vint32mf2_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint64m2_t test___riscv_vwcvt_x_tum(vbool32_t mask,vint64m2_t merge,vint32m1_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint64m4_t test___riscv_vwcvt_x_tum(vbool16_t mask,vint64m4_t merge,vint32m2_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}


vint64m8_t test___riscv_vwcvt_x_tum(vbool8_t mask,vint64m8_t merge,vint32m4_t src,size_t vl)
{
    return __riscv_vwcvt_x_tum(mask,merge,src,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vwcvt\.x\.x\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
