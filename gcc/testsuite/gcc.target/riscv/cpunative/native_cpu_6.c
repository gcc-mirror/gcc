/* A real /proc/cpuinfo, from a SiFive P870-D.  It is the last of the
   thirty-two blocks that are asked for here, and the ISA string is the
   longest and the most recent of any file in this directory, carrying
   names -- zfbfmin, zvfbfwma, smmpm, ssnpm, svade -- that the others
   do not.  */

/* { dg-do compile } */
/* { dg-require-effective-target riscv_native_cpu_detect } */
/* { dg-require-effective-target rv64 } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/riscv/cpunative/info_4" } */
/* { dg-set-compiler-env-var GCC_CPUINFO_CPU "31" } */
/* { dg-additional-options "-march=native -mabi=lp64d -fverbose-asm" } */

int
main (void)
{
  return 0;
}

/* { dg-final { scan-assembler {\.attribute arch, "rv64i2p1[^"]*_zfbfmin1p0_} } } */
/* { dg-final { scan-assembler {\.attribute arch, "[^"]*_zvfbfwma1p0_} } } */
/* { dg-final { scan-assembler {\.attribute arch, "[^"]*_svade1p0_} } } */
