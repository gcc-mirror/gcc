/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" } */

#include "riscv_vector.h"

size_t
test_3 (size_t vl)
{
  return __riscv_vsetvl_e8m4 (vl); /* { dg-error {built-in function '__riscv_vsetvl_e8m4\(vl\)' requires the 'v' ISA extension} } */
}
