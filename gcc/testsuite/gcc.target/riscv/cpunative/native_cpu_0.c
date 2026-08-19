/* The X100 half of a heterogeneous machine.  Its hart implements H, and
   the compiler is told it is running on that core.  */

/* { dg-do compile } */
/* { dg-require-effective-target riscv_native_cpu_detect } */
/* { dg-require-effective-target rv64 } */
/* { dg-set-compiler-env-var GCC_CPUINFO "$srcdir/gcc.target/riscv/cpunative/info_0" } */
/* { dg-set-compiler-env-var GCC_CPUINFO_CPU "0" } */
/* { dg-additional-options "-march=native -mabi=lp64d -fverbose-asm" } */

int
main (void)
{
  return 0;
}

/* { dg-final { scan-assembler {\.attribute arch, "rv64i2p1[^"]*_h1p0_} } } */
