/* { dg-options "-O2 -march=rv32if -mabi=ilp32f" } */
/* { dg-do compile } */

int f()
{
  return __builtin_riscv_frflags();
}
