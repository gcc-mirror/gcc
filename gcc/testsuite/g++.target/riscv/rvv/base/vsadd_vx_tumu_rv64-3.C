/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vsadd_tumu(vbool64_t mask,vint8mf8_t merge,vint8mf8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint8mf4_t test___riscv_vsadd_tumu(vbool32_t mask,vint8mf4_t merge,vint8mf4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint8mf2_t test___riscv_vsadd_tumu(vbool16_t mask,vint8mf2_t merge,vint8mf2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint8m1_t test___riscv_vsadd_tumu(vbool8_t mask,vint8m1_t merge,vint8m1_t op1,int8_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint8m2_t test___riscv_vsadd_tumu(vbool4_t mask,vint8m2_t merge,vint8m2_t op1,int8_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint8m4_t test___riscv_vsadd_tumu(vbool2_t mask,vint8m4_t merge,vint8m4_t op1,int8_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint8m8_t test___riscv_vsadd_tumu(vbool1_t mask,vint8m8_t merge,vint8m8_t op1,int8_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint16mf4_t test___riscv_vsadd_tumu(vbool64_t mask,vint16mf4_t merge,vint16mf4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint16mf2_t test___riscv_vsadd_tumu(vbool32_t mask,vint16mf2_t merge,vint16mf2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint16m1_t test___riscv_vsadd_tumu(vbool16_t mask,vint16m1_t merge,vint16m1_t op1,int16_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint16m2_t test___riscv_vsadd_tumu(vbool8_t mask,vint16m2_t merge,vint16m2_t op1,int16_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint16m4_t test___riscv_vsadd_tumu(vbool4_t mask,vint16m4_t merge,vint16m4_t op1,int16_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint16m8_t test___riscv_vsadd_tumu(vbool2_t mask,vint16m8_t merge,vint16m8_t op1,int16_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint32mf2_t test___riscv_vsadd_tumu(vbool64_t mask,vint32mf2_t merge,vint32mf2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint32m1_t test___riscv_vsadd_tumu(vbool32_t mask,vint32m1_t merge,vint32m1_t op1,int32_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint32m2_t test___riscv_vsadd_tumu(vbool16_t mask,vint32m2_t merge,vint32m2_t op1,int32_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint32m4_t test___riscv_vsadd_tumu(vbool8_t mask,vint32m4_t merge,vint32m4_t op1,int32_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint32m8_t test___riscv_vsadd_tumu(vbool4_t mask,vint32m8_t merge,vint32m8_t op1,int32_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint64m1_t test___riscv_vsadd_tumu(vbool64_t mask,vint64m1_t merge,vint64m1_t op1,int64_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint64m2_t test___riscv_vsadd_tumu(vbool32_t mask,vint64m2_t merge,vint64m2_t op1,int64_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint64m4_t test___riscv_vsadd_tumu(vbool16_t mask,vint64m4_t merge,vint64m4_t op1,int64_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}


vint64m8_t test___riscv_vsadd_tumu(vbool8_t mask,vint64m8_t merge,vint64m8_t op1,int64_t op2,size_t vl)
{
    return __riscv_vsadd_tumu(mask,merge,op1,op2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*tu,\s*mu\s+vsadd\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+,\s*v0.t\s+} 1 } } */
