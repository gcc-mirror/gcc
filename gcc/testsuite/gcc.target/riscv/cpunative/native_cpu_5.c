/* Real /proc/cpuinfo from a Banana Pi BPI-F3.  */

/* { dg-do compile } */
/* { dg-require-effective-target riscv_native_cpu_detect } */
/* { dg-require-effective-target rv64 } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/riscv/cpunative/info_3" } */
/* { dg-set-compiler-env-var GCC_CPUINFO_CPU "0" } */
/* { dg-additional-options "-march=native -mabi=lp64d -fverbose-asm" } */

int
main (void)
{
  return 0;
}

/* { dg-final { scan-assembler {\.attribute arch, "rv64i2p1[^"]*_v1p0_[^"]*_zvfh1p0_} } } */
