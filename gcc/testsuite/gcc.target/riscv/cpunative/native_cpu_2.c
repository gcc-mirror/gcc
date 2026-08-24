/* { dg-do compile } */
/* { dg-require-effective-target riscv_native_cpu_detect } */
/* { dg-require-effective-target rv64 } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/riscv/cpunative/info_0" } */
/* { dg-set-compiler-env-var GCC_CPUINFO_CPU "99" } */
/* { dg-additional-options "-march=native -mtune=native -mabi=lp64d -fverbose-asm" } */

int
main (void)
{
  return 0;
}

/* { dg-final { scan-assembler {\.attribute arch, "rv64i2p1[^"]*_h1p0_} } } */
/* { dg-final { scan-assembler {options passed:[^\n]* -mtune=spacemit-x100} } } */
