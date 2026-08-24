/* { dg-do compile } */
/* { dg-require-effective-target riscv_native_cpu_detect } */
/* { dg-require-effective-target rv64 } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/riscv/cpunative/info_1" } */
/* { dg-set-compiler-env-var GCC_CPUINFO_CPU "0" } */
/* { dg-additional-options "-march=unset -mcpu=native -mabi=lp64d -fverbose-asm" } */

int
main (void)
{
  return 0;
}

/* { dg-final { scan-assembler {\.attribute arch, "rv64i2p1[^"]*_zba1p0_zbb1p0} } } */
/* { dg-final { scan-assembler-not {znosuchext} } } */
/* { dg-final { scan-assembler-not {xmadeup} } } */
/* { dg-final { scan-assembler-not {options passed:[^\n]* -mtune=} } } */
