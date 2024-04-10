/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvknhb -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vuint32mf2_t test_vsha2cl_vv_u32mf2(vuint32mf2_t vd, vuint32mf2_t vs2, vuint32mf2_t vs1, size_t vl) {
  return __riscv_vsha2cl_vv_u32mf2(vd, vs2, vs1, vl);
}

vuint32mf2_t test_vsha2cl_vv_u32mf2_tu(vuint32mf2_t vd, vuint32mf2_t vs2, vuint32mf2_t vs1, size_t vl) {
  return __riscv_vsha2cl_vv_u32mf2_tu(vd, vs2, vs1, vl);
}

vuint32m1_t test_vsha2ch_vv_u32m1(vuint32m1_t vd, vuint32m1_t vs2, vuint32m1_t vs1, size_t vl) {
  return __riscv_vsha2ch_vv_u32m1(vd, vs2, vs1, vl);
}

vuint32m2_t test_vsha2ch_vv_u32m2_tu(vuint32m2_t vd, vuint32m2_t vs2, vuint32m2_t vs1, size_t vl) {
  return __riscv_vsha2ch_vv_u32m2_tu(vd, vs2, vs1, vl);
}

vuint32m2_t test_vsha2ms_vv_u32m2(vuint32m2_t vd, vuint32m2_t vs2, vuint32m2_t vs1, size_t vl) {
  return __riscv_vsha2ms_vv_u32m2(vd, vs2, vs1, vl);
}

vuint64m8_t test_vsha2ms_vv_u64m8_tu(vuint64m8_t vd, vuint64m8_t vs2, vuint64m8_t vs1, size_t vl) {
  return __riscv_vsha2ms_vv_u64m8_tu(vd, vs2, vs1, vl);
}
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*ta,\s*ma} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s*zero,\s*[a-x0-9]+,\s*[a-x0-9]+,m[a-x0-9]+,\s*tu,\s*ma} 3 } } */
/* { dg-final { scan-assembler-times {vsha2cl\.vv\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vsha2ch\.vv\s+v[0-9]+,\s*v[0-9]} 2 } } */
/* { dg-final { scan-assembler-times {vsha2ms\.vv\s+v[0-9]+,\s*v[0-9]} 2 } } */
