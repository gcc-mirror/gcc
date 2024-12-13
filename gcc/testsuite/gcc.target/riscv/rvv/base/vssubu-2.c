/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gcv -mabi=ilp32d" } */

#include <riscv_vector.h>

vuint64m1_t test_vssubu_vx_u64m1(vuint64m1_t op1)
{
  return __riscv_vssubu_vx_u64m1(op1,0,0);
}

/* { dg-final { scan-assembler-not {\tvssubu} } } */
