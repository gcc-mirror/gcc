/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vnclip_wv_i8mf8_tumu(vbool64_t mask,vint8mf8_t merge,vint16mf4_t op1,vuint8mf8_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i8mf8_tumu(mask,merge,op1,shift,32);
}


vint8mf4_t test___riscv_vnclip_wv_i8mf4_tumu(vbool32_t mask,vint8mf4_t merge,vint16mf2_t op1,vuint8mf4_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i8mf4_tumu(mask,merge,op1,shift,32);
}


vint8mf2_t test___riscv_vnclip_wv_i8mf2_tumu(vbool16_t mask,vint8mf2_t merge,vint16m1_t op1,vuint8mf2_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i8mf2_tumu(mask,merge,op1,shift,32);
}


vint8m1_t test___riscv_vnclip_wv_i8m1_tumu(vbool8_t mask,vint8m1_t merge,vint16m2_t op1,vuint8m1_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i8m1_tumu(mask,merge,op1,shift,32);
}


vint8m2_t test___riscv_vnclip_wv_i8m2_tumu(vbool4_t mask,vint8m2_t merge,vint16m4_t op1,vuint8m2_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i8m2_tumu(mask,merge,op1,shift,32);
}


vint8m4_t test___riscv_vnclip_wv_i8m4_tumu(vbool2_t mask,vint8m4_t merge,vint16m8_t op1,vuint8m4_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i8m4_tumu(mask,merge,op1,shift,32);
}


vint16mf4_t test___riscv_vnclip_wv_i16mf4_tumu(vbool64_t mask,vint16mf4_t merge,vint32mf2_t op1,vuint16mf4_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i16mf4_tumu(mask,merge,op1,shift,32);
}


vint16mf2_t test___riscv_vnclip_wv_i16mf2_tumu(vbool32_t mask,vint16mf2_t merge,vint32m1_t op1,vuint16mf2_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i16mf2_tumu(mask,merge,op1,shift,32);
}


vint16m1_t test___riscv_vnclip_wv_i16m1_tumu(vbool16_t mask,vint16m1_t merge,vint32m2_t op1,vuint16m1_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i16m1_tumu(mask,merge,op1,shift,32);
}


vint16m2_t test___riscv_vnclip_wv_i16m2_tumu(vbool8_t mask,vint16m2_t merge,vint32m4_t op1,vuint16m2_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i16m2_tumu(mask,merge,op1,shift,32);
}


vint16m4_t test___riscv_vnclip_wv_i16m4_tumu(vbool4_t mask,vint16m4_t merge,vint32m8_t op1,vuint16m4_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i16m4_tumu(mask,merge,op1,shift,32);
}


vint32mf2_t test___riscv_vnclip_wv_i32mf2_tumu(vbool64_t mask,vint32mf2_t merge,vint64m1_t op1,vuint32mf2_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i32mf2_tumu(mask,merge,op1,shift,32);
}


vint32m1_t test___riscv_vnclip_wv_i32m1_tumu(vbool32_t mask,vint32m1_t merge,vint64m2_t op1,vuint32m1_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i32m1_tumu(mask,merge,op1,shift,32);
}


vint32m2_t test___riscv_vnclip_wv_i32m2_tumu(vbool16_t mask,vint32m2_t merge,vint64m4_t op1,vuint32m2_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i32m2_tumu(mask,merge,op1,shift,32);
}


vint32m4_t test___riscv_vnclip_wv_i32m4_tumu(vbool8_t mask,vint32m4_t merge,vint64m8_t op1,vuint32m4_t shift,size_t vl)
{
    return __riscv_vnclip_wv_i32m4_tumu(mask,merge,op1,shift,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*mu\s+vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
