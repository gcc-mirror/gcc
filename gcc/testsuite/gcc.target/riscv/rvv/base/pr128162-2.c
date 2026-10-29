/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvabd -mabi=lp64d -O2" } */
/* PR target/128162 */

#include "riscv_vector.h"

vuint32m2_t
test_vwabdau_vs2_u32m2 (vuint32m2_t vd, vuint16m1_t vs, size_t vl)
{
  vuint16m2_t t = __riscv_vreinterpret_v_u32m2_u16m2 (vd);
  vuint16m1_t hi = __riscv_vget_v_u16m2_u16m1 (t, 1);
  return __riscv_vwabdau_vv_u32m2 (vd, hi, vs, vl);
}

vuint32m2_t
test_vwabdau_vs1_u32m2 (vuint32m2_t vd, vuint16m1_t vs, size_t vl)
{
  vuint16m2_t t = __riscv_vreinterpret_v_u32m2_u16m2 (vd);
  vuint16m1_t hi = __riscv_vget_v_u16m2_u16m1 (t, 1);
  return __riscv_vwabdau_vv_u32m2 (vd, vs, hi, vl);
}

vuint32m4_t
test_vwabdau_vs2_u32m4 (vuint32m4_t vd, vuint16m2_t vs, size_t vl)
{
  vuint16m4_t t = __riscv_vreinterpret_v_u32m4_u16m4 (vd);
  vuint16m2_t hi = __riscv_vget_v_u16m4_u16m2 (t, 1);
  return __riscv_vwabdau_vv_u32m4 (vd, hi, vs, vl);
}

vuint32m4_t
test_vwabdau_vs1_u32m4 (vuint32m4_t vd, vuint16m2_t vs, size_t vl)
{
  vuint16m4_t t = __riscv_vreinterpret_v_u32m4_u16m4 (vd);
  vuint16m2_t hi = __riscv_vget_v_u16m4_u16m2 (t, 1);
  return __riscv_vwabdau_vv_u32m4 (vd, vs, hi, vl);
}

vuint32m8_t
test_vwabdau_vs2_u32m8 (vuint32m8_t vd, vuint16m4_t vs, size_t vl)
{
  vuint16m8_t t = __riscv_vreinterpret_v_u32m8_u16m8 (vd);
  vuint16m4_t hi = __riscv_vget_v_u16m8_u16m4 (t, 1);
  return __riscv_vwabdau_vv_u32m8 (vd, hi, vs, vl);
}

vuint32m8_t
test_vwabdau_vs1_u32m8 (vuint32m8_t vd, vuint16m4_t vs, size_t vl)
{
  vuint16m8_t t = __riscv_vreinterpret_v_u32m8_u16m8 (vd);
  vuint16m4_t hi = __riscv_vget_v_u16m8_u16m4 (t, 1);
  return __riscv_vwabdau_vv_u32m8 (vd, vs, hi, vl);
}

vuint16m2_t
test_vwabdau_vs2_u16m2 (vuint16m2_t vd, vuint8m1_t vs, size_t vl)
{
  vuint8m2_t t = __riscv_vreinterpret_v_u16m2_u8m2 (vd);
  vuint8m1_t hi = __riscv_vget_v_u8m2_u8m1 (t, 1);
  return __riscv_vwabdau_vv_u16m2 (vd, hi, vs, vl);
}

vuint16m2_t
test_vwabdau_vs1_u16m2 (vuint16m2_t vd, vuint8m1_t vs, size_t vl)
{
  vuint8m2_t t = __riscv_vreinterpret_v_u16m2_u8m2 (vd);
  vuint8m1_t hi = __riscv_vget_v_u8m2_u8m1 (t, 1);
  return __riscv_vwabdau_vv_u16m2 (vd, vs, hi, vl);
}

vuint16m4_t
test_vwabdau_vs2_u16m4 (vuint16m4_t vd, vuint8m2_t vs, size_t vl)
{
  vuint8m4_t t = __riscv_vreinterpret_v_u16m4_u8m4 (vd);
  vuint8m2_t hi = __riscv_vget_v_u8m4_u8m2 (t, 1);
  return __riscv_vwabdau_vv_u16m4 (vd, hi, vs, vl);
}

vuint16m4_t
test_vwabdau_vs1_u16m4 (vuint16m4_t vd, vuint8m2_t vs, size_t vl)
{
  vuint8m4_t t = __riscv_vreinterpret_v_u16m4_u8m4 (vd);
  vuint8m2_t hi = __riscv_vget_v_u8m4_u8m2 (t, 1);
  return __riscv_vwabdau_vv_u16m4 (vd, vs, hi, vl);
}

vuint16m8_t
test_vwabdau_vs2_u16m8 (vuint16m8_t vd, vuint8m4_t vs, size_t vl)
{
  vuint8m8_t t = __riscv_vreinterpret_v_u16m8_u8m8 (vd);
  vuint8m4_t hi = __riscv_vget_v_u8m8_u8m4 (t, 1);
  return __riscv_vwabdau_vv_u16m8 (vd, hi, vs, vl);
}

vuint16m8_t
test_vwabdau_vs1_u16m8 (vuint16m8_t vd, vuint8m4_t vs, size_t vl)
{
  vuint8m8_t t = __riscv_vreinterpret_v_u16m8_u8m8 (vd);
  vuint8m4_t hi = __riscv_vget_v_u8m8_u8m4 (t, 1);
  return __riscv_vwabdau_vv_u16m8 (vd, vs, hi, vl);
}

/* { dg-final { scan-assembler-times {vwabdau\.vv\s+} 12 } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v8,v9,} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v8,v[0-9]+,v9([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v8,v10,} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v8,v[0-9]+,v10([^0-9]|$)} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v8,v12,} } } */
/* { dg-final { scan-assembler-not {vwabdau\.vv\s+v8,v[0-9]+,v12([^0-9]|$)} } } */
