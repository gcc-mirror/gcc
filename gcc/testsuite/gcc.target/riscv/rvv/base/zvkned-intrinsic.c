/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvkned_zve64x -mabi=lp64d -O2" } */
#include "riscv_vector.h"

vuint32mf2_t test_vaesdf_vv_u32mf2(vuint32mf2_t vd, vuint32mf2_t vs2, size_t vl) {
  return __riscv_vaesdf_vv_u32mf2(vd, vs2, vl);
}

vuint32mf2_t test_vaesdf_vs_u32mf2_u32mf2(vuint32mf2_t vd, vuint32mf2_t vs2, size_t vl) {
  return __riscv_vaesdf_vs_u32mf2_u32mf2(vd, vs2, vl);
}

vuint32m2_t test_vaesdf_vv_u32m2_tu(vuint32m2_t vd, vuint32m2_t vs2, size_t vl) {
  return __riscv_vaesdf_vv_u32m2_tu(vd, vs2, vl);
}

vuint32m2_t test_vaesdf_vs_u32m2_u32m2_tu(vuint32m2_t vd, vuint32m2_t vs2, size_t vl) {
  return __riscv_vaesdf_vs_u32m2_u32m2_tu(vd, vs2, vl);
}

vuint32m1_t test_vaesdm_vv_u32m1(vuint32m1_t vd, vuint32m1_t vs2, size_t vl) {
  return __riscv_vaesdm_vv_u32m1(vd, vs2, vl);
}

vuint32m4_t test_vaesdm_vs_u32m1_u32m4(vuint32m4_t vd, vuint32m1_t vs2, size_t vl) {
  return __riscv_vaesdm_vs_u32m1_u32m4(vd, vs2, vl);
}

vuint32m1_t test_vaesdm_vv_u32m1_tu(vuint32m1_t vd, vuint32m1_t vs2, size_t vl) {
  return __riscv_vaesdm_vv_u32m1_tu(vd, vs2, vl);
}

vuint32m2_t test_vaesdm_vs_u32m1_u32m2_tu(vuint32m2_t vd, vuint32m1_t vs2, size_t vl) {
  return __riscv_vaesdm_vs_u32m1_u32m2_tu(vd, vs2, vl);
}

vuint32m2_t test_vaesef_vv_u32m2(vuint32m2_t vd, vuint32m2_t vs2, size_t vl) {
  return __riscv_vaesef_vv_u32m2(vd, vs2, vl);
}

vuint32m2_t test_vaesef_vs_u32m2_u32m2(vuint32m2_t vd, vuint32m2_t vs2, size_t vl) {
  return __riscv_vaesef_vs_u32m2_u32m2(vd, vs2, vl);
}

vuint32m4_t test_vaesef_vv_u32m4_tu(vuint32m4_t vd, vuint32m4_t vs2, size_t vl) {
  return __riscv_vaesef_vv_u32m4_tu(vd, vs2, vl);
}

vuint32m8_t test_vaesef_vs_u32m4_u32m8_tu(vuint32m8_t vd, vuint32m4_t vs2, size_t vl) {
  return __riscv_vaesef_vs_u32m4_u32m8_tu(vd, vs2, vl);
}

vuint32m8_t test_vaesem_vv_u32m8(vuint32m8_t vd, vuint32m8_t vs2, size_t vl) {
  return __riscv_vaesem_vv_u32m8(vd, vs2, vl);
}

vuint32m8_t test_vaesem_vs_u32m8_u32m8(vuint32m8_t vd, vuint32m8_t vs2, size_t vl) {
  return __riscv_vaesem_vs_u32m8_u32m8(vd, vs2, vl);
}

vuint32mf2_t test_vaesem_vv_u32mf2_tu(vuint32mf2_t vd, vuint32mf2_t vs2, size_t vl) {
  return __riscv_vaesem_vv_u32mf2_tu(vd, vs2, vl);
}

vuint32m8_t test_vaesem_vs_u32mf2_u32m8_tu(vuint32m8_t vd, vuint32mf2_t vs2, size_t vl) {
  return __riscv_vaesem_vs_u32mf2_u32m8_tu(vd, vs2, vl);
}

vuint32mf2_t test_vaeskf1_vi_u32mf2(vuint32mf2_t vs2, size_t vl) {
  return __riscv_vaeskf1_vi_u32mf2(vs2, 0, vl);
}

vuint32m1_t test_vaeskf1_vi_u32m1_tu(vuint32m1_t maskedoff, vuint32m1_t vs2, size_t vl) {
  return __riscv_vaeskf1_vi_u32m1_tu(maskedoff, vs2, 0, vl);
}

vuint32m2_t test_vaeskf2_vi_u32m2(vuint32m2_t vd, vuint32m2_t vs2, size_t vl) {
  return __riscv_vaeskf2_vi_u32m2(vd, vs2, 0, vl);
}

vuint32m4_t test_vaeskf2_vi_u32m4_tu(vuint32m4_t vd, vuint32m4_t vs2, size_t vl) {
  return __riscv_vaeskf2_vi_u32m4_tu(vd, vs2, 0, vl);
}

vuint32m4_t test_vaesz_vs_u32m1_u32m4(vuint32m4_t vd, vuint32m1_t vs2, size_t vl) {
  return __riscv_vaesz_vs_u32m1_u32m4(vd, vs2, vl);
}

vuint32m8_t test_vaesz_vs_u32m1_u32m8_tu(vuint32m8_t vd, vuint32m1_t vs2, size_t vl) {
  return __riscv_vaesz_vs_u32m1_u32m8_tu(vd, vs2, vl);
}
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*ta,\s*ma} 11 } } */
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*tu,\s*ma} 11 } } */
/* { dg-final { scan-assembler-times {vaesdf\.vv\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vaesdf\.vs\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vaesdm\.vv\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vaesdm\.vs\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vaesef\.vv\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vaesef\.vs\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vaesem\.vv\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vaesem\.vs\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vaeskf1\.vi\s+v[0-9]+,\s*v[0-9]+,0} 2 } } */
/* { dg-final { scan-assembler-times {vaeskf2\.vi\s+v[0-9]+,\s*v[0-9]+,0} 2 } } */
/* { dg-final { scan-assembler-times {vaesz\.vs\s+v[0-9]+,\s*v[0-9]} 2 } } */
