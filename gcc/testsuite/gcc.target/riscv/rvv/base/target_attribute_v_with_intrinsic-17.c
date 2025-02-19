/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vint64m1_t
__attribute__((target("arch=+zve32x")))
test_1 (vint64m1_t a, vint64m1_t b, size_t vl)
{
  return __riscv_vadd_vv_i64m1 (a, b, vl);
}

/* { dg-error "return type 'vint64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension" "" { target { "riscv*-*-*" } } 0 } */
/* { dg-error "argument type 'vint64m1_t' requires the zve64x, zve64f, zve64d or v ISA extension" "" { target { "riscv*-*-*" } } 0 } */
