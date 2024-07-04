/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vuint32m1_t
__attribute__((target("arch=+v")))
test_1 (vuint32m1_t op_1, size_t vl)
{
  return __riscv_vclz_v_u32m1 (op_1, vl); /* { dg-error {built-in function '__riscv_vclz_v_u32m1\(op_1,  vl\)' requires the 'zvbb' ISA extension} } */
}
