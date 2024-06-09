/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvksh_zve64x -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vuint32mf2_t test_vsm3c_vi_u32mf2(vuint32mf2_t vd, vuint32mf2_t vs2, size_t vl) {
  return __riscv_vsm3c_vi_u32mf2(vd, vs2, 0, vl);
}

vuint32m1_t test_vsm3c_vi_u32m1_tu(vuint32m1_t vd, vuint32m1_t vs2, size_t vl) {
  return __riscv_vsm3c_vi_u32m1_tu(vd, vs2, 0, vl);
}

vuint32m2_t test_vsm3me_vv_u32m2(vuint32m2_t vs2, vuint32m2_t vs1, size_t vl) {
  return __riscv_vsm3me_vv_u32m2(vs2, vs1, vl);
}

vuint32m4_t test_vsm3me_vv_u32m4_tu(vuint32m4_t maskedoff, vuint32m4_t vs2, vuint32m4_t vs1, size_t vl) {
  return __riscv_vsm3me_vv_u32m4_tu(maskedoff, vs2, vs1, vl);
}
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*ta,\s*ma} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*tu,\s*ma} 2 } } */
/* { dg-final { scan-assembler-times {vsm3c\.vi\s+v[0-9]+,\s*v[0-9]+,0} 2 } } */
/* { dg-final { scan-assembler-times {vsm3me\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 2 } } */
