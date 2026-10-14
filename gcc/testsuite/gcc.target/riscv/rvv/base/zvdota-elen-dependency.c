/* { dg-do compile } */
/* { dg-options "-march=rv64im_zvqwdota16i -mabi=lp64 -O2" } */

#include "riscv_vector.h"

/* Zvqwdota16i implies Zve64x, including fractional e16,mf4 support.  */
#ifndef __riscv_zve64x
#error "Zvqwdota16i must imply Zve64x"
#endif

vint64m1_t
test_i16mf4 (vint64m1_t acc, vint16mf4_t a, vint16mf4_t b, size_t vl)
{
  return __riscv_vqwdota_vv_i16mf4_i16mf4_i64m1 (acc, a, b, vl);
}

/* { dg-final { scan-assembler {vqwdotas\.vv} } } */
/* { dg-final { scan-assembler {e16alt,\s*mf4,} } } */
