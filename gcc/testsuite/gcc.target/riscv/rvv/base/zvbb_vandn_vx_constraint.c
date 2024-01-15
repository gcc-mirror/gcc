/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zvbb_zve64x -mabi=ilp32 -O3" } */
#include "riscv_vector.h"

vuint64m1_t test_vandn_vx_u64m1(vuint64m1_t vs2, uint64_t rs1, size_t vl) {
  return __riscv_vandn_vx_u64m1(vs2, rs1, vl);
}

vuint64m1_t test_vandn_vx_u64m1_extend(vuint64m1_t vs2, size_t vl) {
  uint32_t rs1 = 0x12345678;
  return __riscv_vandn_vx_u64m1(vs2, rs1, vl);
}

/* { dg-final { scan-assembler-times {vandn\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]} 1 } } */
/* { dg-final { scan-assembler-times {vandn\.vx\s+v[0-9]+,\s*v[0-9]+,\s*a[0-9]} 1 } } */
