/* { dg-do compile } */
/* { dg-require-effective-target riscv_native_cpu_detect } */
/* { dg-require-effective-target rv64 } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/riscv/mcpu-native-1.cpuinfo" } */
/* { dg-options "-mcpu=native -fverbose-asm" } */

int
main (void)
{
  return 0;
}

/* { dg-final { scan-assembler {-mcpu=sifive-p870-d} } } */
