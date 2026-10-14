/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vint32m1_t
test_i8 (vint32m1_t acc, vint8m1_t a, size_t vl)
{
  return __riscv_vqwdota_vv_i8m1_i8m1_i32m1 (acc, a, a, vl); /* { dg-error {requires.*zvqwdota8i} } */
}
