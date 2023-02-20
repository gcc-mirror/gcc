/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint16mf4_t test___riscv_vwmacc_vv_i16mf4_m(vbool64_t mask,vint16mf4_t vd,vint8mf8_t vs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16mf4_m(mask,vd,vs1,vs2,32);
}


vint16mf2_t test___riscv_vwmacc_vv_i16mf2_m(vbool32_t mask,vint16mf2_t vd,vint8mf4_t vs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16mf2_m(mask,vd,vs1,vs2,32);
}


vint16m1_t test___riscv_vwmacc_vv_i16m1_m(vbool16_t mask,vint16m1_t vd,vint8mf2_t vs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16m1_m(mask,vd,vs1,vs2,32);
}


vint16m2_t test___riscv_vwmacc_vv_i16m2_m(vbool8_t mask,vint16m2_t vd,vint8m1_t vs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16m2_m(mask,vd,vs1,vs2,32);
}


vint16m4_t test___riscv_vwmacc_vv_i16m4_m(vbool4_t mask,vint16m4_t vd,vint8m2_t vs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16m4_m(mask,vd,vs1,vs2,32);
}


vint16m8_t test___riscv_vwmacc_vv_i16m8_m(vbool2_t mask,vint16m8_t vd,vint8m4_t vs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i16m8_m(mask,vd,vs1,vs2,32);
}


vint32mf2_t test___riscv_vwmacc_vv_i32mf2_m(vbool64_t mask,vint32mf2_t vd,vint16mf4_t vs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i32mf2_m(mask,vd,vs1,vs2,32);
}


vint32m1_t test___riscv_vwmacc_vv_i32m1_m(vbool32_t mask,vint32m1_t vd,vint16mf2_t vs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i32m1_m(mask,vd,vs1,vs2,32);
}


vint32m2_t test___riscv_vwmacc_vv_i32m2_m(vbool16_t mask,vint32m2_t vd,vint16m1_t vs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i32m2_m(mask,vd,vs1,vs2,32);
}


vint32m4_t test___riscv_vwmacc_vv_i32m4_m(vbool8_t mask,vint32m4_t vd,vint16m2_t vs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i32m4_m(mask,vd,vs1,vs2,32);
}


vint32m8_t test___riscv_vwmacc_vv_i32m8_m(vbool4_t mask,vint32m8_t vd,vint16m4_t vs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i32m8_m(mask,vd,vs1,vs2,32);
}


vint64m1_t test___riscv_vwmacc_vv_i64m1_m(vbool64_t mask,vint64m1_t vd,vint32mf2_t vs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i64m1_m(mask,vd,vs1,vs2,32);
}


vint64m2_t test___riscv_vwmacc_vv_i64m2_m(vbool32_t mask,vint64m2_t vd,vint32m1_t vs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i64m2_m(mask,vd,vs1,vs2,32);
}


vint64m4_t test___riscv_vwmacc_vv_i64m4_m(vbool16_t mask,vint64m4_t vd,vint32m2_t vs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i64m4_m(mask,vd,vs1,vs2,32);
}


vint64m8_t test___riscv_vwmacc_vv_i64m8_m(vbool8_t mask,vint64m8_t vd,vint32m4_t vs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vwmacc_vv_i64m8_m(mask,vd,vs1,vs2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vwmacc\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
