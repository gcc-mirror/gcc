/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvkg_zve64x -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vuint32mf2_t test_vgmul_vv_u32mf2(vuint32mf2_t vd, vuint32mf2_t vs2, size_t vl) {
  return __riscv_vgmul_vv_u32mf2(vd, vs2, vl);
}

vuint32m1_t test_vgmul_vv_u32m1_tu(vuint32m1_t vd, vuint32m1_t vs2, size_t vl) {
  return __riscv_vgmul_vv_u32m1_tu(vd, vs2, vl);
}

vuint32m2_t test_vghsh_vv_u32m2(vuint32m2_t vd, vuint32m2_t vs2, vuint32m2_t vs1, size_t vl) {
  return __riscv_vghsh_vv_u32m2(vd, vs2, vs1, vl);
}

vuint32m4_t test_vghsh_vv_u32m4_tu(vuint32m4_t vd, vuint32m4_t vs2, vuint32m4_t vs1, size_t vl) {
  return __riscv_vghsh_vv_u32m4_tu(vd, vs2, vs1, vl);
}
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*ta,\s*ma} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*tu,\s*ma} 2 } } */
/* { dg-final { scan-assembler-times {vgmul\.vv\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vghsh\.vv\s+v[0-9]+,\s*v[0-9]} 2 } } */
