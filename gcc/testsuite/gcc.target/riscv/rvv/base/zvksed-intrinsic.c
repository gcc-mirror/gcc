/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvksed_zve64x -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vuint32mf2_t test_vsm4k_vi_u32mf2(vuint32mf2_t vs2, size_t vl) {
  return __riscv_vsm4k_vi_u32mf2(vs2, 0, vl);
}

vuint32m1_t test_vsm4k_vi_u32m1_tu(vuint32m1_t maskedoff, vuint32m1_t vs2, size_t vl) {
  return __riscv_vsm4k_vi_u32m1_tu(maskedoff, vs2, 0, vl);
}

vuint32m2_t test_vsm4r_vv_u32m2(vuint32m2_t vd, vuint32m2_t vs2, size_t vl) {
  return __riscv_vsm4r_vv_u32m2(vd, vs2, vl);
}

vuint32m4_t test_vsm4r_vv_u32m4_tu(vuint32m4_t vd, vuint32m4_t vs2, size_t vl) {
  return __riscv_vsm4r_vv_u32m4_tu(vd, vs2, vl);
}

vuint32m4_t test_vsm4r_vs_u32mf2_u32m4(vuint32m4_t vd, vuint32mf2_t vs2, size_t vl) {
  return __riscv_vsm4r_vs_u32mf2_u32m4(vd, vs2, vl);
}

vuint32m8_t test_vsm4r_vs_u32m1_u32m8_tu(vuint32m8_t vd, vuint32m1_t vs2, size_t vl) {
  return __riscv_vsm4r_vs_u32m1_u32m8_tu(vd, vs2, vl);
}
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*ta,\s*ma} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*tu,\s*ma} 3 } } */
/* { dg-final { scan-assembler-times {vsm4k\.vi\s+v[0-9]+,\s*v[0-9]+,0} 2 } } */
/* { dg-final { scan-assembler-times {vsm4r\.vv\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vsm4r\.vs\s+v[0-9]+,\s*v[0-9]} 2 } } */
