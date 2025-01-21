/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xtheadvector -mabi=ilp32 -O0" } */

#include "riscv_vector.h"

vint32m1_t
prefix (vint32m1_t vx, vint32m1_t vy, size_t vl)
{
  return __riscv_vadd_vv_i32m1 (vx, vy, vl);
}

/* { dg-final { scan-assembler {\mth\.vadd\.vv\M} } } */
