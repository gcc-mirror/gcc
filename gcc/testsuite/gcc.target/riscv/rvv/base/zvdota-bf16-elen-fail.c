/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f_zvl64b_zvfwdota16bf_zvfbfmin -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vfloat32m1_t
test_bf16mf4 (vfloat32m1_t acc, vbfloat16mf4_t a, size_t vl)
{
  return __riscv_vfwdota_vv_bf16mf4_bf16mf4_f32m1 (acc, a, a, vl); /* { dg-error {requires.*zve64x} } */
}
