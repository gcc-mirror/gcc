/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vfloat32m1_t
test_f32 (vfloat32m1_t acc, vfloat32m8_t a, vfloat32m1_t b, size_t vl)
{
  return __riscv_vfbdota_vv_f32m1 (acc, a, b, 0, vl); /* { dg-error {requires.*zvfbdota32f} } */
}
