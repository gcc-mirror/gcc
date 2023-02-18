/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vnmsub_mu(vbool64_t mask,vint8mf8_t vd,vint8mf8_t vs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint8mf4_t test___riscv_vnmsub_mu(vbool32_t mask,vint8mf4_t vd,vint8mf4_t vs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint8mf2_t test___riscv_vnmsub_mu(vbool16_t mask,vint8mf2_t vd,vint8mf2_t vs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint8m1_t test___riscv_vnmsub_mu(vbool8_t mask,vint8m1_t vd,vint8m1_t vs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint8m2_t test___riscv_vnmsub_mu(vbool4_t mask,vint8m2_t vd,vint8m2_t vs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint8m4_t test___riscv_vnmsub_mu(vbool2_t mask,vint8m4_t vd,vint8m4_t vs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint8m8_t test___riscv_vnmsub_mu(vbool1_t mask,vint8m8_t vd,vint8m8_t vs1,vint8m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint16mf4_t test___riscv_vnmsub_mu(vbool64_t mask,vint16mf4_t vd,vint16mf4_t vs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint16mf2_t test___riscv_vnmsub_mu(vbool32_t mask,vint16mf2_t vd,vint16mf2_t vs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint16m1_t test___riscv_vnmsub_mu(vbool16_t mask,vint16m1_t vd,vint16m1_t vs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint16m2_t test___riscv_vnmsub_mu(vbool8_t mask,vint16m2_t vd,vint16m2_t vs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint16m4_t test___riscv_vnmsub_mu(vbool4_t mask,vint16m4_t vd,vint16m4_t vs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint16m8_t test___riscv_vnmsub_mu(vbool2_t mask,vint16m8_t vd,vint16m8_t vs1,vint16m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint32mf2_t test___riscv_vnmsub_mu(vbool64_t mask,vint32mf2_t vd,vint32mf2_t vs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint32m1_t test___riscv_vnmsub_mu(vbool32_t mask,vint32m1_t vd,vint32m1_t vs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint32m2_t test___riscv_vnmsub_mu(vbool16_t mask,vint32m2_t vd,vint32m2_t vs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint32m4_t test___riscv_vnmsub_mu(vbool8_t mask,vint32m4_t vd,vint32m4_t vs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint32m8_t test___riscv_vnmsub_mu(vbool4_t mask,vint32m8_t vd,vint32m8_t vs1,vint32m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint64m1_t test___riscv_vnmsub_mu(vbool64_t mask,vint64m1_t vd,vint64m1_t vs1,vint64m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint64m2_t test___riscv_vnmsub_mu(vbool32_t mask,vint64m2_t vd,vint64m2_t vs1,vint64m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint64m4_t test___riscv_vnmsub_mu(vbool16_t mask,vint64m4_t vd,vint64m4_t vs1,vint64m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vint64m8_t test___riscv_vnmsub_mu(vbool8_t mask,vint64m8_t vd,vint64m8_t vs1,vint64m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint8mf8_t test___riscv_vnmsub_mu(vbool64_t mask,vuint8mf8_t vd,vuint8mf8_t vs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint8mf4_t test___riscv_vnmsub_mu(vbool32_t mask,vuint8mf4_t vd,vuint8mf4_t vs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint8mf2_t test___riscv_vnmsub_mu(vbool16_t mask,vuint8mf2_t vd,vuint8mf2_t vs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint8m1_t test___riscv_vnmsub_mu(vbool8_t mask,vuint8m1_t vd,vuint8m1_t vs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint8m2_t test___riscv_vnmsub_mu(vbool4_t mask,vuint8m2_t vd,vuint8m2_t vs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint8m4_t test___riscv_vnmsub_mu(vbool2_t mask,vuint8m4_t vd,vuint8m4_t vs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint8m8_t test___riscv_vnmsub_mu(vbool1_t mask,vuint8m8_t vd,vuint8m8_t vs1,vuint8m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint16mf4_t test___riscv_vnmsub_mu(vbool64_t mask,vuint16mf4_t vd,vuint16mf4_t vs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint16mf2_t test___riscv_vnmsub_mu(vbool32_t mask,vuint16mf2_t vd,vuint16mf2_t vs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint16m1_t test___riscv_vnmsub_mu(vbool16_t mask,vuint16m1_t vd,vuint16m1_t vs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint16m2_t test___riscv_vnmsub_mu(vbool8_t mask,vuint16m2_t vd,vuint16m2_t vs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint16m4_t test___riscv_vnmsub_mu(vbool4_t mask,vuint16m4_t vd,vuint16m4_t vs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint16m8_t test___riscv_vnmsub_mu(vbool2_t mask,vuint16m8_t vd,vuint16m8_t vs1,vuint16m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint32mf2_t test___riscv_vnmsub_mu(vbool64_t mask,vuint32mf2_t vd,vuint32mf2_t vs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint32m1_t test___riscv_vnmsub_mu(vbool32_t mask,vuint32m1_t vd,vuint32m1_t vs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint32m2_t test___riscv_vnmsub_mu(vbool16_t mask,vuint32m2_t vd,vuint32m2_t vs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint32m4_t test___riscv_vnmsub_mu(vbool8_t mask,vuint32m4_t vd,vuint32m4_t vs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint32m8_t test___riscv_vnmsub_mu(vbool4_t mask,vuint32m8_t vd,vuint32m8_t vs1,vuint32m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint64m1_t test___riscv_vnmsub_mu(vbool64_t mask,vuint64m1_t vd,vuint64m1_t vs1,vuint64m1_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint64m2_t test___riscv_vnmsub_mu(vbool32_t mask,vuint64m2_t vd,vuint64m2_t vs1,vuint64m2_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint64m4_t test___riscv_vnmsub_mu(vbool16_t mask,vuint64m4_t vd,vuint64m4_t vs1,vuint64m4_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}


vuint64m8_t test___riscv_vnmsub_mu(vbool8_t mask,vuint64m8_t vd,vuint64m8_t vs1,vuint64m8_t vs2,size_t vl)
{
    return __riscv_vnmsub_mu(mask,vd,vs1,vs2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*t[au],\s*mu\s+vnms[a-u][b-c]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
