/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vsra_tu(vint8mf8_t merge,vint8mf8_t op1,vuint8mf8_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint8mf4_t test___riscv_vsra_tu(vint8mf4_t merge,vint8mf4_t op1,vuint8mf4_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint8mf2_t test___riscv_vsra_tu(vint8mf2_t merge,vint8mf2_t op1,vuint8mf2_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint8m1_t test___riscv_vsra_tu(vint8m1_t merge,vint8m1_t op1,vuint8m1_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint8m2_t test___riscv_vsra_tu(vint8m2_t merge,vint8m2_t op1,vuint8m2_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint8m4_t test___riscv_vsra_tu(vint8m4_t merge,vint8m4_t op1,vuint8m4_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint8m8_t test___riscv_vsra_tu(vint8m8_t merge,vint8m8_t op1,vuint8m8_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint16mf4_t test___riscv_vsra_tu(vint16mf4_t merge,vint16mf4_t op1,vuint16mf4_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint16mf2_t test___riscv_vsra_tu(vint16mf2_t merge,vint16mf2_t op1,vuint16mf2_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint16m1_t test___riscv_vsra_tu(vint16m1_t merge,vint16m1_t op1,vuint16m1_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint16m2_t test___riscv_vsra_tu(vint16m2_t merge,vint16m2_t op1,vuint16m2_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint16m4_t test___riscv_vsra_tu(vint16m4_t merge,vint16m4_t op1,vuint16m4_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint16m8_t test___riscv_vsra_tu(vint16m8_t merge,vint16m8_t op1,vuint16m8_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint32mf2_t test___riscv_vsra_tu(vint32mf2_t merge,vint32mf2_t op1,vuint32mf2_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint32m1_t test___riscv_vsra_tu(vint32m1_t merge,vint32m1_t op1,vuint32m1_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint32m2_t test___riscv_vsra_tu(vint32m2_t merge,vint32m2_t op1,vuint32m2_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint32m4_t test___riscv_vsra_tu(vint32m4_t merge,vint32m4_t op1,vuint32m4_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint32m8_t test___riscv_vsra_tu(vint32m8_t merge,vint32m8_t op1,vuint32m8_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint64m1_t test___riscv_vsra_tu(vint64m1_t merge,vint64m1_t op1,vuint64m1_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint64m2_t test___riscv_vsra_tu(vint64m2_t merge,vint64m2_t op1,vuint64m2_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint64m4_t test___riscv_vsra_tu(vint64m4_t merge,vint64m4_t op1,vuint64m4_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}


vint64m8_t test___riscv_vsra_tu(vint64m8_t merge,vint64m8_t op1,vuint64m8_t shift,size_t vl)
{
    return __riscv_vsra_tu(merge,op1,shift,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*tu,\s*m[au]\s+vsra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
