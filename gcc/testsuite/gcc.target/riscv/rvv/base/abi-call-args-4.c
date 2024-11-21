/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O1" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

/*
** foo1:
**   ...
**   vwadd\.vv\tv\d+,v\d+,v\d+
**   ...
**   vwsub\.vv\tv\d+,v\d+,v\d+
**   ...
**   vwmul\.vv\tv\d+,v\d+,v\d+
**   ...
**   vand\.vv\tv8,v\d+,v\d+
**   ...
*/
vint64m8_t
foo1 (vint8m1_t a1, vint8m1_t a2, vint16m2_t b1, vint32m4_t c1, vint64m8_t d1,
      size_t vl)
{
  vint16m2_t b2 = __riscv_vwadd_vv_i16m2 (a1, a2, vl);
  vint32m4_t c2 = __riscv_vwsub_vv_i32m4 (b1, b2, vl);
  vint64m8_t d2 = __riscv_vwmul_vv_i64m8 (c1, c2, vl);
  return __riscv_vand_vv_i64m8 (d1, d2, vl);
}

/*
** foo2:
**   ...
**   vwadd\.vv\tv\d+,v\d+,v\d+
**   ...
**   vwsub\.vv\tv\d+,v\d+,v\d+
**   ...
**   vwmul\.vv\tv\d+,v\d+,v\d+
**   ...
**   vand\.vv\tv8,v\d+,v\d+
**   ...
*/
vint64m8_t
foo2 (vint8m1_t a1, vint16m2_t b1, vint8m1_t a2, vint32m4_t c1, vint64m8_t d1,
      size_t vl)
{
  vint16m2_t b2 = __riscv_vwadd_vv_i16m2 (a1, a2, vl);
  vint32m4_t c2 = __riscv_vwsub_vv_i32m4 (b1, b2, vl);
  vint64m8_t d2 = __riscv_vwmul_vv_i64m8 (c1, c2, vl);
  return __riscv_vand_vv_i64m8 (d1, d2, vl);
}

/*
** foo3:
**   ...
**   vwadd\.vv\tv\d+,v\d+,v\d+
**   ...
**   vwsub\.vv\tv\d+,v\d+,v\d+
**   ...
**   vwmul\.vv\tv\d+,v\d+,v\d+
**   ...
**   vand\.vv\tv8,v\d+,v\d+
**   ...
*/
vint64m8_t
foo3 (vint8m1_t a1, vint16m2_t b1, vint32m4_t c1, vint8m1_t a2, vint64m8_t d1,
      size_t vl)
{
  vint16m2_t b2 = __riscv_vwadd_vv_i16m2 (a1, a2, vl);
  vint32m4_t c2 = __riscv_vwsub_vv_i32m4 (b1, b2, vl);
  vint64m8_t d2 = __riscv_vwmul_vv_i64m8 (c1, c2, vl);
  return __riscv_vand_vv_i64m8 (d1, d2, vl);
}

/*
** foo4:
**   ...
**   vwadd\.vv\tv\d+,v\d+,v\d+
**   ...
**   vwsub\.vv\tv\d+,v\d+,v\d+
**   ...
**   vwmul\.vv\tv\d+,v\d+,v\d+
**   ...
**   vand\.vv\tv8,v\d+,v\d+
**   ...
*/
vint64m8_t
foo4 (vint8m1_t a1, vint16m2_t b1, vint32m4_t c1, vint64m8_t d1, vint8m1_t a2,
      size_t vl)
{
  vint16m2_t b2 = __riscv_vwadd_vv_i16m2 (a1, a2, vl);
  vint32m4_t c2 = __riscv_vwsub_vv_i32m4 (b1, b2, vl);
  vint64m8_t d2 = __riscv_vwmul_vv_i64m8 (c1, c2, vl);
  return __riscv_vand_vv_i64m8 (d1, d2, vl);
}

/*
** foo5:
**   vsetivli\tzero,0,e8,m1,ta,ma
**   vmv1r\.v\tv8,v16
**   vmv1r\.v\tv9,v17
**   vmv1r\.v\tv10,v18
**   vmv1r\.v\tv11,v19
**   vmv1r\.v\tv12,v20
**   vmv1r\.v\tv13,v21
**   vmv1r\.v\tv14,v22
**   vmv1r\.v\tv15,v23
**   ...
*/
vint8m1x8_t
foo5 (vint8m8_t a, vint8m1x8_t b)
{
  return b;
}
