/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vnsra_wv_i8mf8(vint16mf4_t op1,vuint8mf8_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i8mf8(op1,shift,vl);
}


vint8mf4_t test___riscv_vnsra_wv_i8mf4(vint16mf2_t op1,vuint8mf4_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i8mf4(op1,shift,vl);
}


vint8mf2_t test___riscv_vnsra_wv_i8mf2(vint16m1_t op1,vuint8mf2_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i8mf2(op1,shift,vl);
}


vint8m1_t test___riscv_vnsra_wv_i8m1(vint16m2_t op1,vuint8m1_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i8m1(op1,shift,vl);
}


vint8m2_t test___riscv_vnsra_wv_i8m2(vint16m4_t op1,vuint8m2_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i8m2(op1,shift,vl);
}


vint8m4_t test___riscv_vnsra_wv_i8m4(vint16m8_t op1,vuint8m4_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i8m4(op1,shift,vl);
}


vint16mf4_t test___riscv_vnsra_wv_i16mf4(vint32mf2_t op1,vuint16mf4_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i16mf4(op1,shift,vl);
}


vint16mf2_t test___riscv_vnsra_wv_i16mf2(vint32m1_t op1,vuint16mf2_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i16mf2(op1,shift,vl);
}


vint16m1_t test___riscv_vnsra_wv_i16m1(vint32m2_t op1,vuint16m1_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i16m1(op1,shift,vl);
}


vint16m2_t test___riscv_vnsra_wv_i16m2(vint32m4_t op1,vuint16m2_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i16m2(op1,shift,vl);
}


vint16m4_t test___riscv_vnsra_wv_i16m4(vint32m8_t op1,vuint16m4_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i16m4(op1,shift,vl);
}


vint32mf2_t test___riscv_vnsra_wv_i32mf2(vint64m1_t op1,vuint32mf2_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i32mf2(op1,shift,vl);
}


vint32m1_t test___riscv_vnsra_wv_i32m1(vint64m2_t op1,vuint32m1_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i32m1(op1,shift,vl);
}


vint32m2_t test___riscv_vnsra_wv_i32m2(vint64m4_t op1,vuint32m2_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i32m2(op1,shift,vl);
}


vint32m4_t test___riscv_vnsra_wv_i32m4(vint64m8_t op1,vuint32m4_t shift,size_t vl)
{
    return __riscv_vnsra_wv_i32m4(op1,shift,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vnsra\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
