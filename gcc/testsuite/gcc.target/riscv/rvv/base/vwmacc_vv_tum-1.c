/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmacc_vv_i16mf4_tum(vbool64_t mask,vint16mf4_t vd,vint8mf8_t vs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16mf4_tum(mask,vd,vs1,vs2,vl);
}


vint16mf2_t test___riscv_vwmacc_vv_i16mf2_tum(vbool32_t mask,vint16mf2_t vd,vint8mf4_t vs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16mf2_tum(mask,vd,vs1,vs2,vl);
}


vint16m1_t test___riscv_vwmacc_vv_i16m1_tum(vbool16_t mask,vint16m1_t vd,vint8mf2_t vs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16m1_tum(mask,vd,vs1,vs2,vl);
}


vint16m2_t test___riscv_vwmacc_vv_i16m2_tum(vbool8_t mask,vint16m2_t vd,vint8m1_t vs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16m2_tum(mask,vd,vs1,vs2,vl);
}


vint16m4_t test___riscv_vwmacc_vv_i16m4_tum(vbool4_t mask,vint16m4_t vd,vint8m2_t vs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16m4_tum(mask,vd,vs1,vs2,vl);
}


vint16m8_t test___riscv_vwmacc_vv_i16m8_tum(vbool2_t mask,vint16m8_t vd,vint8m4_t vs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16m8_tum(mask,vd,vs1,vs2,vl);
}


vint32mf2_t test___riscv_vwmacc_vv_i32mf2_tum(vbool64_t mask,vint32mf2_t vd,vint16mf4_t vs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i32mf2_tum(mask,vd,vs1,vs2,vl);
}


vint32m1_t test___riscv_vwmacc_vv_i32m1_tum(vbool32_t mask,vint32m1_t vd,vint16mf2_t vs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i32m1_tum(mask,vd,vs1,vs2,vl);
}


vint32m2_t test___riscv_vwmacc_vv_i32m2_tum(vbool16_t mask,vint32m2_t vd,vint16m1_t vs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i32m2_tum(mask,vd,vs1,vs2,vl);
}


vint32m4_t test___riscv_vwmacc_vv_i32m4_tum(vbool8_t mask,vint32m4_t vd,vint16m2_t vs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i32m4_tum(mask,vd,vs1,vs2,vl);
}


vint32m8_t test___riscv_vwmacc_vv_i32m8_tum(vbool4_t mask,vint32m8_t vd,vint16m4_t vs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i32m8_tum(mask,vd,vs1,vs2,vl);
}


vint64m1_t test___riscv_vwmacc_vv_i64m1_tum(vbool64_t mask,vint64m1_t vd,vint32mf2_t vs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i64m1_tum(mask,vd,vs1,vs2,vl);
}


vint64m2_t test___riscv_vwmacc_vv_i64m2_tum(vbool32_t mask,vint64m2_t vd,vint32m1_t vs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i64m2_tum(mask,vd,vs1,vs2,vl);
}


vint64m4_t test___riscv_vwmacc_vv_i64m4_tum(vbool16_t mask,vint64m4_t vd,vint32m2_t vs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i64m4_tum(mask,vd,vs1,vs2,vl);
}


vint64m8_t test___riscv_vwmacc_vv_i64m8_tum(vbool8_t mask,vint64m8_t vd,vint32m4_t vs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i64m8_tum(mask,vd,vs1,vs2,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
