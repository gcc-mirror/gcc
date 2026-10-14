/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvqwdota8i_zvqwdota16i -mabi=lp64d -O2" } */

#include "riscv_vector.h"

/* The accumulator remains LMUL=1 even for the largest source groups.  */
vint32m1_t
test_i8m8 (vint32m1_t acc, vint8m8_t a, vint8m8_t b, size_t vl)
{
  return __riscv_vqwdota_vv_i8m8_i8m8_i32m1 (acc, a, b, vl);
}

vint64m1_t
test_i16m4 (vint64m1_t acc, vint16m4_t a, vint16m4_t b, size_t vl)
{
  return __riscv_vqwdota_vv_i16m4_i16m4_i64m1 (acc, a, b, vl);
}

/* { dg-final { scan-assembler-times {vqwdotas\.vv} 2 } } */
/* { dg-final { scan-assembler {e8alt,\s*m8,} } } */
/* { dg-final { scan-assembler {e16alt,\s*m4,} } } */
