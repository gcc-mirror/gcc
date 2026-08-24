/* { dg-do compile } */
/* { dg-require-effective-target riscv_native_cpu_detect } */
/* { dg-options "-march=unset -mcpu=native" } */

#ifndef __riscv_arch_test
#error "-mcpu=native did not produce a usable ISA string"
#endif

int
main (void)
{
  return 0;
}
