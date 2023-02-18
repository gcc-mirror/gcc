/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vuint64m1_t test___riscv_vzext_vf8(vuint8mf8_t op1,size_t vl)
{
    return __riscv_vzext_vf8(op1,vl);
}


vuint64m2_t test___riscv_vzext_vf8(vuint8mf4_t op1,size_t vl)
{
    return __riscv_vzext_vf8(op1,vl);
}


vuint64m4_t test___riscv_vzext_vf8(vuint8mf2_t op1,size_t vl)
{
    return __riscv_vzext_vf8(op1,vl);
}


vuint64m8_t test___riscv_vzext_vf8(vuint8m1_t op1,size_t vl)
{
    return __riscv_vzext_vf8(op1,vl);
}


vuint64m1_t test___riscv_vzext_vf8(vbool64_t mask,vuint8mf8_t op1,size_t vl)
{
    return __riscv_vzext_vf8(mask,op1,vl);
}


vuint64m2_t test___riscv_vzext_vf8(vbool32_t mask,vuint8mf4_t op1,size_t vl)
{
    return __riscv_vzext_vf8(mask,op1,vl);
}


vuint64m4_t test___riscv_vzext_vf8(vbool16_t mask,vuint8mf2_t op1,size_t vl)
{
    return __riscv_vzext_vf8(mask,op1,vl);
}


vuint64m8_t test___riscv_vzext_vf8(vbool8_t mask,vuint8m1_t op1,size_t vl)
{
    return __riscv_vzext_vf8(mask,op1,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vzext\.vf8\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 1 } } */
