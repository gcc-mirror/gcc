/* Test that we do not have error when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32x -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vint64m1_t
__attribute__((target("arch=+v")))
test_1 (vint64m1_t a, vint64m1_t b, size_t vl)
{
  return __riscv_vadd_vv_i64m1 (a, b, vl);
}
