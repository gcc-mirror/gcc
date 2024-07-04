/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=zvl" } */

#include "single_rgroup-2.c"

int main (void)
{
  TEST_ALL (run_1)
  return 0;
}
