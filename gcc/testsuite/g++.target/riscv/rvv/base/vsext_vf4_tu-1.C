/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint32mf2_t test___riscv_vsext_vf4_tu(vint32mf2_t merge,vint8mf8_t op1,size_t vl)
{
    return __riscv_vsext_vf4_tu(merge,op1,vl);
}


vint32m1_t test___riscv_vsext_vf4_tu(vint32m1_t merge,vint8mf4_t op1,size_t vl)
{
    return __riscv_vsext_vf4_tu(merge,op1,vl);
}


vint32m2_t test___riscv_vsext_vf4_tu(vint32m2_t merge,vint8mf2_t op1,size_t vl)
{
    return __riscv_vsext_vf4_tu(merge,op1,vl);
}


vint32m4_t test___riscv_vsext_vf4_tu(vint32m4_t merge,vint8m1_t op1,size_t vl)
{
    return __riscv_vsext_vf4_tu(merge,op1,vl);
}


vint32m8_t test___riscv_vsext_vf4_tu(vint32m8_t merge,vint8m2_t op1,size_t vl)
{
    return __riscv_vsext_vf4_tu(merge,op1,vl);
}


vint64m1_t test___riscv_vsext_vf4_tu(vint64m1_t merge,vint16mf4_t op1,size_t vl)
{
    return __riscv_vsext_vf4_tu(merge,op1,vl);
}


vint64m2_t test___riscv_vsext_vf4_tu(vint64m2_t merge,vint16mf2_t op1,size_t vl)
{
    return __riscv_vsext_vf4_tu(merge,op1,vl);
}


vint64m4_t test___riscv_vsext_vf4_tu(vint64m4_t merge,vint16m1_t op1,size_t vl)
{
    return __riscv_vsext_vf4_tu(merge,op1,vl);
}


vint64m8_t test___riscv_vsext_vf4_tu(vint64m8_t merge,vint16m2_t op1,size_t vl)
{
    return __riscv_vsext_vf4_tu(merge,op1,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*tu,\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*tu,\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*tu,\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*tu,\s*m[au]\s+vsext\.vf4\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
