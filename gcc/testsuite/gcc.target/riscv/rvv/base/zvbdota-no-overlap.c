/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfbdota32f_zvl512b -mabi=lp64d -O2" } */

#include "riscv_vector.h"

/* Equal accumulator and source values still need disjoint registers.  */
vfloat32m1_t
test_same_vs1 (vfloat32m1_t acc, vfloat32m8_t a, size_t vl)
{
  return __riscv_vfbdota_vv_f32m1 (acc, a, acc, 0, vl);
}

vfloat32m8_t
test_same_vs2 (vbool4_t mask, vfloat32m8_t acc, vfloat32m1_t b, size_t vl)
{
  return __riscv_vfbdota_vv_f32m8_m (mask, acc, acc, b, 0, vl);
}

/* { dg-final { scan-assembler-times {vfbdota\.vv} 2 } } */
/* { dg-final { scan-assembler-not {vfbdota\.vv\s+(v[0-9]+),\s*\1,} } } */
/* { dg-final { scan-assembler-not {vfbdota\.vv\s+(v[0-9]+),\s*v[0-9]+,\s*\1,} } } */
/* { dg-final { scan-assembler-not {vfbdota\.vv\s+v0,.*v0\.t} } } */
