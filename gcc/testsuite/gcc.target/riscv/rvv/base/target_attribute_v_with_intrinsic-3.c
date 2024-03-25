/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3 -Wno-implicit-int" } */

#include "riscv_vector.h"

vint32m1_t
__attribute__((target("arch=+zbb")))
test_1 (vint32m1_t a, vint32m1_t b, size_t vl)
{
  return __riscv_vadd_vv_i32m1 (a, b, vl);
}

/* { dg-error "return type 'vint32m1_t' requires the V ISA extension" "" { target { "riscv*-*-*" } } 0 } */
