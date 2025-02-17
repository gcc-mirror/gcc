/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vfloat64m1_t
__attribute__((target("arch=+zve64f")))
test_1 (vfloat64m1_t a, vfloat64m1_t b, size_t vl)
{
  return __riscv_vfadd_vv_f64m1 (a, b, vl);
}

/* { dg-error "return type 'vfloat64m1_t' requires the zve64d or v ISA extension" "" { target { "riscv*-*-*" } } 0 } */
/* { dg-error "argument type 'vfloat64m1_t' requires the zve64d or v ISA extension" "" { target { "riscv*-*-*" } } 0 } */
