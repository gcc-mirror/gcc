/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvbc -mabi=lp64d -O3" } */
#include "riscv_vector.h"

vuint64m1_t test_vclmul_vx_u64m1_extend(vuint64m1_t vs2, uint32_t rs1, size_t vl) {
  return __riscv_vclmul_vx_u64m1(vs2, rs1, vl);
}

vuint64m1_t test_vclmulh_vx_u64m1_extend(vuint64m1_t vs2, uint32_t rs1, size_t vl) {
  return __riscv_vclmulh_vx_u64m1(vs2, rs1, vl);
}

/* { dg-final { scan-assembler-times {vclmul\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]} 1 } } */
/* { dg-final { scan-assembler-times {vclmulh\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]} 1 } } */
