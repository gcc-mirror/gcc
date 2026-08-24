/* As native_cpu_6.c, but asking for both through -mcpu=native.  */

/* { dg-do compile } */
/* { dg-require-effective-target riscv_native_cpu_detect } */
/* { dg-require-effective-target rv64 } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/riscv/cpunative/info_4" } */
/* { dg-set-compiler-env-var GCC_CPUINFO_CPU "31" } */
/* { dg-additional-options "-march=unset -mcpu=native -mabi=lp64d -fverbose-asm" } */

int
main (void)
{
  return 0;
}

/* { dg-final { scan-assembler {\.attribute arch, "rv64i2p1[^"]*_zfbfmin1p0_} } } */
/* { dg-final { scan-assembler {\.attribute arch, "[^"]*_zvfbfwma1p0_} } } */
/* { dg-final { scan-assembler {\.attribute arch, "[^"]*_svade1p0_} } } */
/* { dg-final { scan-assembler {options passed:[^\n]* -mtune=sifive-p870-d} } } */
