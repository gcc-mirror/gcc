/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvbb_zve64x -mabi=lp64d" } */
#include "riscv_vector.h"

vuint8mf8_t test_vandn_vv_u8mf8(vuint8mf8_t vs2, vuint8mf8_t vs1, size_t vl) {
  return __riscv_vandn_vv_u8mf8(vs2, vs1, vl);
}

vuint32m1_t test_vandn_vx_u32m1(vuint32m1_t vs2, uint32_t rs1, size_t vl) {
  return __riscv_vandn_vx_u32m1(vs2, rs1, vl);
}

vuint32m2_t test_vandn_vv_u32m2_m(vbool16_t mask, vuint32m2_t vs2, vuint32m2_t vs1, size_t vl) {
  return __riscv_vandn_vv_u32m2_m(mask, vs2, vs1, vl);
}

vuint16mf2_t test_vandn_vx_u16mf2_m(vbool32_t mask, vuint16mf2_t vs2, uint16_t rs1, size_t vl) {
  return __riscv_vandn_vx_u16mf2_m(mask, vs2, rs1, vl);
}

vuint32m4_t test_vandn_vv_u32m4_tumu(vbool8_t mask, vuint32m4_t maskedoff, vuint32m4_t vs2, vuint32m4_t vs1, size_t vl) {
  return __riscv_vandn_vv_u32m4_tumu(mask, maskedoff, vs2, vs1, vl);
}

vuint64m4_t test_vandn_vx_u64m4_tumu(vbool16_t mask, vuint64m4_t maskedoff, vuint64m4_t vs2, uint64_t rs1, size_t vl) {
  return __riscv_vandn_vx_u64m4_tumu(mask, maskedoff, vs2, rs1, vl);
}

vuint8m8_t test_vbrev_v_u8m8(vuint8m8_t vs2, size_t vl) {
  return __riscv_vbrev_v_u8m8(vs2, vl);
}

vuint16m1_t test_vbrev_v_u16m1_m(vbool16_t mask, vuint16m1_t vs2, size_t vl) {
  return __riscv_vbrev_v_u16m1_m(mask, vs2, vl);
}

vuint32m4_t test_vbrev_v_u32m4_tumu(vbool8_t mask, vuint32m4_t maskedoff, vuint32m4_t vs2, size_t vl) {
  return __riscv_vbrev_v_u32m4_tumu(mask, maskedoff, vs2, vl);
}

vuint16mf4_t test_vbrev8_v_u16mf4(vuint16mf4_t vs2, size_t vl) {
  return __riscv_vbrev8_v_u16mf4(vs2, vl);
}

vuint32m1_t test_vbrev8_v_u32m1_m(vbool32_t mask, vuint32m1_t vs2, size_t vl) {
  return __riscv_vbrev8_v_u32m1_m(mask, vs2, vl);
}

vuint64m1_t test_vbrev8_v_u64m1_tumu(vbool64_t mask, vuint64m1_t maskedoff, vuint64m1_t vs2, size_t vl) {
  return __riscv_vbrev8_v_u64m1_tumu(mask, maskedoff, vs2, vl);
}

vuint16m4_t test_vrev8_v_u16m4(vuint16m4_t vs2, size_t vl) {
  return __riscv_vrev8_v_u16m4(vs2, vl);
}

vuint8m4_t test_vrev8_v_u8m4_m(vbool2_t mask, vuint8m4_t vs2, size_t vl) {
  return __riscv_vrev8_v_u8m4_m(mask, vs2, vl);
}

vuint32m1_t test_vrev8_v_u32m1_tumu(vbool32_t mask, vuint32m1_t maskedoff, vuint32m1_t vs2, size_t vl) {
  return __riscv_vrev8_v_u32m1_tumu(mask, maskedoff, vs2, vl);
}

vuint8m8_t test_vrol_vv_u8m8(vuint8m8_t vs2, vuint8m8_t vs1, size_t vl) {
  return __riscv_vrol_vv_u8m8(vs2, vs1, vl);
}

vuint16m4_t test_vrol_vx_u16m4(vuint16m4_t vs2, size_t rs1, size_t vl) {
  return __riscv_vrol_vx_u16m4(vs2, rs1, vl);
}

vuint16mf2_t test_vrol_vv_u16mf2_m(vbool32_t mask, vuint16mf2_t vs2, vuint16mf2_t vs1, size_t vl) {
  return __riscv_vrol_vv_u16mf2_m(mask, vs2, vs1, vl);
}

vuint64m1_t test_vrol_vx_u64m1_m(vbool64_t mask, vuint64m1_t vs2, size_t rs1, size_t vl) {
  return __riscv_vrol_vx_u64m1_m(mask, vs2, rs1, vl);
}

vuint8m1_t test_vrol_vv_u8m1_tumu(vbool8_t mask, vuint8m1_t maskedoff, vuint8m1_t vs2, vuint8m1_t vs1, size_t vl) {
  return __riscv_vrol_vv_u8m1_tumu(mask, maskedoff, vs2, vs1, vl);
}

vuint16m2_t test_vrol_vx_u16m2_tumu(vbool8_t mask, vuint16m2_t maskedoff, vuint16m2_t vs2, size_t rs1, size_t vl) {
  return __riscv_vrol_vx_u16m2_tumu(mask, maskedoff, vs2, rs1, vl);
}

vuint8m8_t test_vror_vv_u8m8(vuint8m8_t vs2, vuint8m8_t vs1, size_t vl) {
  return __riscv_vror_vv_u8m8(vs2, vs1, vl);
}

vuint32m2_t test_vror_vx_u32m2(vuint32m2_t vs2, size_t rs1, size_t vl) {
  return __riscv_vror_vx_u32m2(vs2, rs1, vl);
}

vuint16mf2_t test_vror_vv_u16mf2_m(vbool32_t mask, vuint16mf2_t vs2, vuint16mf2_t vs1, size_t vl) {
  return __riscv_vror_vv_u16mf2_m(mask, vs2, vs1, vl);
}

vuint16m1_t test_vror_vx_u16m1_m(vbool16_t mask, vuint16m1_t vs2, size_t rs1, size_t vl) {
  return __riscv_vror_vx_u16m1_m(mask, vs2, rs1, vl);
}

vuint16mf2_t test_vror_vv_u16mf2_tumu(vbool32_t mask, vuint16mf2_t maskedoff, vuint16mf2_t vs2, vuint16mf2_t vs1, size_t vl) {
  return __riscv_vror_vv_u16mf2_tumu(mask, maskedoff, vs2, vs1, vl);
}

vuint64m1_t test_vror_vx_u64m1_tumu(vbool64_t mask, vuint64m1_t maskedoff, vuint64m1_t vs2, size_t rs1, size_t vl) {
  return __riscv_vror_vx_u64m1_tumu(mask, maskedoff, vs2, rs1, vl);
}

vuint8m2_t test_vclz_v_u8m2(vuint8m2_t vs2, size_t vl) {
  return __riscv_vclz_v_u8m2(vs2, vl);
}

vuint64m2_t test_vclz_v_u64m2_m(vbool32_t mask, vuint64m2_t vs2, size_t vl) {
  return __riscv_vclz_v_u64m2_m(mask, vs2, vl);
}

vuint16mf4_t test_vctz_v_u16mf4(vuint16mf4_t vs2, size_t vl) {
  return __riscv_vctz_v_u16mf4(vs2, vl);
}

vuint32m8_t test_vctz_v_u32m8_m(vbool4_t mask, vuint32m8_t vs2, size_t vl) {
  return __riscv_vctz_v_u32m8_m(mask, vs2, vl);
}

vuint16mf4_t test_vwsll_vx_u16mf4(vuint8mf8_t vs2, size_t rs1, size_t vl) {
  return __riscv_vwsll_vx_u16mf4(vs2, rs1, vl);
}

vuint16m1_t test_vwsll_vv_u16m1(vuint8mf2_t vs2, vuint8mf2_t vs1, size_t vl) {
  return __riscv_vwsll_vv_u16m1(vs2, vs1, vl);
}

vuint32m2_t test_vwsll_vv_u32m2_m(vbool16_t mask, vuint16m1_t vs2, vuint16m1_t vs1, size_t vl) {
  return __riscv_vwsll_vv_u32m2_m(mask, vs2, vs1, vl);
}

vuint32m2_t test_vwsll_vx_u32m2_m(vbool16_t mask, vuint16m1_t vs2, size_t rs1, size_t vl) {
  return __riscv_vwsll_vx_u32m2_m(mask, vs2, rs1, vl);
}

vuint16mf4_t test_vwsll_vv_u16mf4_tumu(vbool64_t mask, vuint16mf4_t maskedoff, vuint8mf8_t vs2, vuint8mf8_t vs1, size_t vl) {
  return __riscv_vwsll_vv_u16mf4_tumu(mask, maskedoff, vs2, vs1, vl);
}

vuint16mf4_t test_vwsll_vx_u16mf4_tumu(vbool64_t mask, vuint16mf4_t maskedoff, vuint8mf8_t vs2, size_t rs1, size_t vl) {
  return __riscv_vwsll_vx_u16mf4_tumu(mask, maskedoff, vs2, rs1, vl);
}
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*ta,\s*ma} 26 } } */
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*tu,\s*mu} 11 } } */
/* { dg-final { scan-assembler-times {vandn\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vandn\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vandn\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vandn\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vbrev\.v\s+v[0-9]+,\s*v[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vbrev\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vbrev8\.v\s+v[0-9]+,\s*v[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vbrev8\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vrev8\.v\s+v[0-9]+,\s*v[0-9]} 3} } */
/* { dg-final { scan-assembler-times {vrev8\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vrol\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vrol\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vrol\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vrol\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vror\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vror\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vror\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vror\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vclz\.v\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vclz\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vctz\.v\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vctz\.v\s+v[0-9]+,\s*v[0-9]+,\s*v0.t} 1 } } */
/* { dg-final { scan-assembler-times {vwsll\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vwsll\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vwsll\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vwsll\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]+,\s*v0.t} 2 } } */
