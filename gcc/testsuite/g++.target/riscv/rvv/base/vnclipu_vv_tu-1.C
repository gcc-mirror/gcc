/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint8mf8_t test___riscv_vnclipu_tu(vuint8mf8_t merge,vuint16mf4_t op1,vuint8mf8_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint8mf4_t test___riscv_vnclipu_tu(vuint8mf4_t merge,vuint16mf2_t op1,vuint8mf4_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint8mf2_t test___riscv_vnclipu_tu(vuint8mf2_t merge,vuint16m1_t op1,vuint8mf2_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint8m1_t test___riscv_vnclipu_tu(vuint8m1_t merge,vuint16m2_t op1,vuint8m1_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint8m2_t test___riscv_vnclipu_tu(vuint8m2_t merge,vuint16m4_t op1,vuint8m2_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint8m4_t test___riscv_vnclipu_tu(vuint8m4_t merge,vuint16m8_t op1,vuint8m4_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint16mf4_t test___riscv_vnclipu_tu(vuint16mf4_t merge,vuint32mf2_t op1,vuint16mf4_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint16mf2_t test___riscv_vnclipu_tu(vuint16mf2_t merge,vuint32m1_t op1,vuint16mf2_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint16m1_t test___riscv_vnclipu_tu(vuint16m1_t merge,vuint32m2_t op1,vuint16m1_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint16m2_t test___riscv_vnclipu_tu(vuint16m2_t merge,vuint32m4_t op1,vuint16m2_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint16m4_t test___riscv_vnclipu_tu(vuint16m4_t merge,vuint32m8_t op1,vuint16m4_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint32mf2_t test___riscv_vnclipu_tu(vuint32mf2_t merge,vuint64m1_t op1,vuint32mf2_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint32m1_t test___riscv_vnclipu_tu(vuint32m1_t merge,vuint64m2_t op1,vuint32m1_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint32m2_t test___riscv_vnclipu_tu(vuint32m2_t merge,vuint64m4_t op1,vuint32m2_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}


vuint32m4_t test___riscv_vnclipu_tu(vuint32m4_t merge,vuint64m8_t op1,vuint32m4_t shift,size_t vl)
{
    return __riscv_vnclipu_tu(merge,op1,shift,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+\s+} 1 } } */
