/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvbc -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vuint64m1_t test_vclmul_vv_u64m1(vuint64m1_t vs2, vuint64m1_t vs1, size_t vl) {
  return __riscv_vclmul_vv_u64m1(vs2, vs1, vl);
}

vuint64m1_t test_vclmul_vx_u64m1(vuint64m1_t vs2, uint64_t rs1, size_t vl) {
  return __riscv_vclmul_vx_u64m1(vs2, rs1, vl);
}

vuint64m2_t test_vclmul_vv_u64m2_m(vbool32_t mask, vuint64m2_t vs2, vuint64m2_t vs1, size_t vl) {
  return __riscv_vclmul_vv_u64m2_m(mask, vs2, vs1, vl);
}

vuint64m2_t test_vclmul_vx_u64m2_m(vbool32_t mask, vuint64m2_t vs2, uint64_t rs1, size_t vl) {
  return __riscv_vclmul_vx_u64m2_m(mask, vs2, rs1, vl);
}

vuint64m4_t test_vclmul_vv_u64m4_tumu(vbool16_t mask, vuint64m4_t maskedoff, vuint64m4_t vs2, vuint64m4_t vs1, size_t vl) {
  return __riscv_vclmul_vv_u64m4_tumu(mask, maskedoff, vs2, vs1, vl);
}

vuint64m4_t test_vclmul_vx_u64m4_tumu(vbool16_t mask, vuint64m4_t maskedoff, vuint64m4_t vs2, uint64_t rs1, size_t vl) {
  return __riscv_vclmul_vx_u64m4_tumu(mask, maskedoff, vs2, rs1, vl);
}

vuint64m2_t test_vclmulh_vv_u64m2(vuint64m2_t vs2, vuint64m2_t vs1, size_t vl) {
  return __riscv_vclmulh_vv_u64m2(vs2, vs1, vl);
}

vuint64m2_t test_vclmulh_vx_u64m2(vuint64m2_t vs2, uint64_t rs1, size_t vl) {
  return __riscv_vclmulh_vx_u64m2(vs2, rs1, vl);
}

vuint64m1_t test_vclmulh_vx_u64m1_m(vbool64_t mask, vuint64m1_t vs2, uint64_t rs1, size_t vl) {
  return __riscv_vclmulh_vx_u64m1_m(mask, vs2, rs1, vl);
}

vuint64m2_t test_vclmulh_vv_u64m2_m(vbool32_t mask, vuint64m2_t vs2, vuint64m2_t vs1, size_t vl) {
  return __riscv_vclmulh_vv_u64m2_m(mask, vs2, vs1, vl);
}

vuint64m8_t test_vclmulh_vv_u64m8_tumu(vbool8_t mask, vuint64m8_t maskedoff, vuint64m8_t vs2, vuint64m8_t vs1, size_t vl) {
  return __riscv_vclmulh_vv_u64m8_tumu(mask, maskedoff, vs2, vs1, vl);
}

vuint64m8_t test_vclmulh_vx_u64m8_tumu(vbool8_t mask, vuint64m8_t maskedoff, vuint64m8_t vs2, uint64_t rs1, size_t vl) {
  return __riscv_vclmulh_vx_u64m8_tumu(mask, maskedoff, vs2, rs1, vl);
}
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*ta,\s*ma} 8 } } */
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*tu,\s*mu} 4 } } */
/* { dg-final { scan-assembler-times {vclmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vclmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vclmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vclmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vclmulh\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vclmulh\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t} 2 } } */
/* { dg-final { scan-assembler-times {vclmulh\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]} 3 } } */
/* { dg-final { scan-assembler-times {vclmulh\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]+,\s*v0.t} 2 } } */
