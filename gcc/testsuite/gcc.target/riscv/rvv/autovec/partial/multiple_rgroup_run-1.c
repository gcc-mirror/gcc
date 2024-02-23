/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=zvl" } */

#include "multiple_rgroup-1.c"

int main (void)
{
  TEST_ALL (run_1)
  TEST_ALL (run_2)
  TEST_ALL (run_3)
  TEST_ALL (run_4)
  TEST_ALL (run_5)
  TEST_ALL (run_6)
  TEST_ALL (run_7)
  TEST_ALL (run_8)
  TEST_ALL (run_9)
  TEST_ALL (run_10)
  return 0;
}
