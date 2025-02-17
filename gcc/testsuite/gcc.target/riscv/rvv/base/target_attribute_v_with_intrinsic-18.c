/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vfloat32m1_t
__attribute__((target("arch=+zve32x")))
test_1 (vfloat32m1_t a, vfloat32m1_t b, size_t vl)
{
  return __riscv_vfadd_vv_f32m1 (a, b, vl);
}

/* { dg-error "return type 'vfloat32m1_t' requires the zve32f, zve64f, zve64d or v ISA extension" "" { target { "riscv*-*-*" } } 0 } */
/* { dg-error "argument type 'vfloat32m1_t' requires the zve32f, zve64f, zve64d or v ISA extension" "" { target { "riscv*-*-*" } } 0 } */