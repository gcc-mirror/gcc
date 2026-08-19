/* { dg-do compile } */
/* { dg-require-effective-target riscv_native_cpu_detect } */
/* { dg-options "-march=native" } */

#ifndef __riscv_arch_test
#error "-march=native did not produce a usable ISA string"
#endif

#ifndef __riscv_mul
#error "-march=native lost the M extension"
#endif

#ifndef __riscv_atomic
#error "-march=native lost the A extension"
#endif

int
main (void)
{
  return 0;
}
