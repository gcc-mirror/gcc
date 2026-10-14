/* { dg-do compile } */
/* { dg-options "-march=rv64im_zve32x -mabi=lp64 -O2" } */

#include "riscv_vector.h"

void
test_i16 (vint64m1_t *acc, vint16m1_t a, size_t vl)
{
  *acc = __riscv_vqwdota_vv_i16m1_i16m1_i64m1 (*acc, a, a, vl); /* { dg-error {requires.*zvqwdota16i} } */
}
