/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vncvt_x_tumu(vbool64_t mask,vint8mf8_t merge,vint16mf4_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint8mf4_t test___riscv_vncvt_x_tumu(vbool32_t mask,vint8mf4_t merge,vint16mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint8mf2_t test___riscv_vncvt_x_tumu(vbool16_t mask,vint8mf2_t merge,vint16m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint8m1_t test___riscv_vncvt_x_tumu(vbool8_t mask,vint8m1_t merge,vint16m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint8m2_t test___riscv_vncvt_x_tumu(vbool4_t mask,vint8m2_t merge,vint16m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint8m4_t test___riscv_vncvt_x_tumu(vbool2_t mask,vint8m4_t merge,vint16m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint8mf8_t test___riscv_vncvt_x_tumu(vbool64_t mask,vuint8mf8_t merge,vuint16mf4_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint8mf4_t test___riscv_vncvt_x_tumu(vbool32_t mask,vuint8mf4_t merge,vuint16mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint8mf2_t test___riscv_vncvt_x_tumu(vbool16_t mask,vuint8mf2_t merge,vuint16m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint8m1_t test___riscv_vncvt_x_tumu(vbool8_t mask,vuint8m1_t merge,vuint16m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint8m2_t test___riscv_vncvt_x_tumu(vbool4_t mask,vuint8m2_t merge,vuint16m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint8m4_t test___riscv_vncvt_x_tumu(vbool2_t mask,vuint8m4_t merge,vuint16m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint16mf4_t test___riscv_vncvt_x_tumu(vbool64_t mask,vint16mf4_t merge,vint32mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint16mf2_t test___riscv_vncvt_x_tumu(vbool32_t mask,vint16mf2_t merge,vint32m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint16m1_t test___riscv_vncvt_x_tumu(vbool16_t mask,vint16m1_t merge,vint32m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint16m2_t test___riscv_vncvt_x_tumu(vbool8_t mask,vint16m2_t merge,vint32m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint16m4_t test___riscv_vncvt_x_tumu(vbool4_t mask,vint16m4_t merge,vint32m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint16mf4_t test___riscv_vncvt_x_tumu(vbool64_t mask,vuint16mf4_t merge,vuint32mf2_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint16mf2_t test___riscv_vncvt_x_tumu(vbool32_t mask,vuint16mf2_t merge,vuint32m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint16m1_t test___riscv_vncvt_x_tumu(vbool16_t mask,vuint16m1_t merge,vuint32m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint16m2_t test___riscv_vncvt_x_tumu(vbool8_t mask,vuint16m2_t merge,vuint32m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint16m4_t test___riscv_vncvt_x_tumu(vbool4_t mask,vuint16m4_t merge,vuint32m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint32mf2_t test___riscv_vncvt_x_tumu(vbool64_t mask,vint32mf2_t merge,vint64m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint32m1_t test___riscv_vncvt_x_tumu(vbool32_t mask,vint32m1_t merge,vint64m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint32m2_t test___riscv_vncvt_x_tumu(vbool16_t mask,vint32m2_t merge,vint64m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vint32m4_t test___riscv_vncvt_x_tumu(vbool8_t mask,vint32m4_t merge,vint64m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint32mf2_t test___riscv_vncvt_x_tumu(vbool64_t mask,vuint32mf2_t merge,vuint64m1_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint32m1_t test___riscv_vncvt_x_tumu(vbool32_t mask,vuint32m1_t merge,vuint64m2_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint32m2_t test___riscv_vncvt_x_tumu(vbool16_t mask,vuint32m2_t merge,vuint64m4_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}


vuint32m4_t test___riscv_vncvt_x_tumu(vbool8_t mask,vuint32m4_t merge,vuint64m8_t src,size_t vl)
{
    return __riscv_vncvt_x_tumu(mask,merge,src,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*tu,\s*mu\s+vncvt\.x\.x\.w\s+v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
