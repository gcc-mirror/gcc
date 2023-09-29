/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "--param riscv-autovec-preference=fixed-vlmax" } */

#include "single_rgroup-2.c"

int main (void)
{
  TEST_ALL (run_1)
  return 0;
}
