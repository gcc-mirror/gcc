/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vuint32m1_t
__attribute__((target("arch=+v")))
test_1 (vuint32m1_t dest, vuint32m1_t op_1, vuint32m1_t op_2, size_t vl)
{
  return __riscv_vghsh_vv_u32m1 (dest, op_1, op_2, vl); /* { dg-error {built-in function '__riscv_vghsh_vv_u32m1\(dest,  op_1,  op_2,  vl\)' requires the 'zvkg' ISA extension} } */
}
