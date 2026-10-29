/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvabd -mabi=lp64d -O2" } */
/* PR target/128162 */

#include "riscv_vector.h"

vuint32m2_t
test_vwabda_vs2_u32m2 (vuint32m2_t vd, vint16m1_t vs, size_t vl)
{
  vint32m2_t s = __riscv_vreinterpret_v_u32m2_i32m2 (vd);
  vint16m2_t t = __riscv_vreinterpret_v_i32m2_i16m2 (s);
  vint16m1_t hi = __riscv_vget_v_i16m2_i16m1 (t, 1);
  return __riscv_vwabda_vv_u32m2 (vd, hi, vs, vl);
}

vuint32m2_t
test_vwabda_vs1_u32m2 (vuint32m2_t vd, vint16m1_t vs, size_t vl)
{
  vint32m2_t s = __riscv_vreinterpret_v_u32m2_i32m2 (vd);
  vint16m2_t t = __riscv_vreinterpret_v_i32m2_i16m2 (s);
  vint16m1_t hi = __riscv_vget_v_i16m2_i16m1 (t, 1);
  return __riscv_vwabda_vv_u32m2 (vd, vs, hi, vl);
}

vuint32m4_t
test_vwabda_vs2_u32m4 (vuint32m4_t vd, vint16m2_t vs, size_t vl)
{
  vint32m4_t s = __riscv_vreinterpret_v_u32m4_i32m4 (vd);
  vint16m4_t t = __riscv_vreinterpret_v_i32m4_i16m4 (s);
  vint16m2_t hi = __riscv_vget_v_i16m4_i16m2 (t, 1);
  return __riscv_vwabda_vv_u32m4 (vd, hi, vs, vl);
}

vuint32m4_t
test_vwabda_vs1_u32m4 (vuint32m4_t vd, vint16m2_t vs, size_t vl)
{
  vint32m4_t s = __riscv_vreinterpret_v_u32m4_i32m4 (vd);
  vint16m4_t t = __riscv_vreinterpret_v_i32m4_i16m4 (s);
  vint16m2_t hi = __riscv_vget_v_i16m4_i16m2 (t, 1);
  return __riscv_vwabda_vv_u32m4 (vd, vs, hi, vl);
}

vuint32m8_t
test_vwabda_vs2_u32m8 (vuint32m8_t vd, vint16m4_t vs, size_t vl)
{
  vint32m8_t s = __riscv_vreinterpret_v_u32m8_i32m8 (vd);
  vint16m8_t t = __riscv_vreinterpret_v_i32m8_i16m8 (s);
  vint16m4_t hi = __riscv_vget_v_i16m8_i16m4 (t, 1);
  return __riscv_vwabda_vv_u32m8 (vd, hi, vs, vl);
}

vuint32m8_t
test_vwabda_vs1_u32m8 (vuint32m8_t vd, vint16m4_t vs, size_t vl)
{
  vint32m8_t s = __riscv_vreinterpret_v_u32m8_i32m8 (vd);
  vint16m8_t t = __riscv_vreinterpret_v_i32m8_i16m8 (s);
  vint16m4_t hi = __riscv_vget_v_i16m8_i16m4 (t, 1);
  return __riscv_vwabda_vv_u32m8 (vd, vs, hi, vl);
}

vuint16m2_t
test_vwabda_vs2_u16m2 (vuint16m2_t vd, vint8m1_t vs, size_t vl)
{
  vint16m2_t s = __riscv_vreinterpret_v_u16m2_i16m2 (vd);
  vint8m2_t t = __riscv_vreinterpret_v_i16m2_i8m2 (s);
  vint8m1_t hi = __riscv_vget_v_i8m2_i8m1 (t, 1);
  return __riscv_vwabda_vv_u16m2 (vd, hi, vs, vl);
}

vuint16m2_t
test_vwabda_vs1_u16m2 (vuint16m2_t vd, vint8m1_t vs, size_t vl)
{
  vint16m2_t s = __riscv_vreinterpret_v_u16m2_i16m2 (vd);
  vint8m2_t t = __riscv_vreinterpret_v_i16m2_i8m2 (s);
  vint8m1_t hi = __riscv_vget_v_i8m2_i8m1 (t, 1);
  return __riscv_vwabda_vv_u16m2 (vd, vs, hi, vl);
}

vuint16m4_t
test_vwabda_vs2_u16m4 (vuint16m4_t vd, vint8m2_t vs, size_t vl)
{
  vint16m4_t s = __riscv_vreinterpret_v_u16m4_i16m4 (vd);
  vint8m4_t t = __riscv_vreinterpret_v_i16m4_i8m4 (s);
  vint8m2_t hi = __riscv_vget_v_i8m4_i8m2 (t, 1);
  return __riscv_vwabda_vv_u16m4 (vd, hi, vs, vl);
}

vuint16m4_t
test_vwabda_vs1_u16m4 (vuint16m4_t vd, vint8m2_t vs, size_t vl)
{
  vint16m4_t s = __riscv_vreinterpret_v_u16m4_i16m4 (vd);
  vint8m4_t t = __riscv_vreinterpret_v_i16m4_i8m4 (s);
  vint8m2_t hi = __riscv_vget_v_i8m4_i8m2 (t, 1);
  return __riscv_vwabda_vv_u16m4 (vd, vs, hi, vl);
}

vuint16m8_t
test_vwabda_vs2_u16m8 (vuint16m8_t vd, vint8m4_t vs, size_t vl)
{
  vint16m8_t s = __riscv_vreinterpret_v_u16m8_i16m8 (vd);
  vint8m8_t t = __riscv_vreinterpret_v_i16m8_i8m8 (s);
  vint8m4_t hi = __riscv_vget_v_i8m8_i8m4 (t, 1);
  return __riscv_vwabda_vv_u16m8 (vd, hi, vs, vl);
}

vuint16m8_t
test_vwabda_vs1_u16m8 (vuint16m8_t vd, vint8m4_t vs, size_t vl)
{
  vint16m8_t s = __riscv_vreinterpret_v_u16m8_i16m8 (vd);
  vint8m8_t t = __riscv_vreinterpret_v_i16m8_i8m8 (s);
  vint8m4_t hi = __riscv_vget_v_i8m8_i8m4 (t, 1);
  return __riscv_vwabda_vv_u16m8 (vd, vs, hi, vl);
}

/* { dg-final { scan-assembler-times {vwabda\.vv\s+} 12 } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v8,v9,} } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v8,v[0-9]+,v9([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v8,v10,} } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v8,v[0-9]+,v10([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v8,v12,} } } */
/* { dg-final { scan-assembler-not {vwabda\.vv\s+v8,v[0-9]+,v12([^0-9]|$)} } } */
