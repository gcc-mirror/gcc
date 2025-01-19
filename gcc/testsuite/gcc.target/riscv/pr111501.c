/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Os" "-Og" "-Oz" "-flto" } } */

unsigned int
do_shift(unsigned long csum)
{
  return (unsigned short)(csum >> 32);
}

unsigned int
do_shift2(unsigned long csum)
{
  return (csum << 16) >> 48;
}

/* { dg-final { scan-assembler-times "slli\t" 2 } } */
/* { dg-final { scan-assembler-times "srli\t" 2 } } */
/* { dg-final { scan-assembler-times "slli\ta\[0-9\],a\[0-9\],16" 2 } } */
/* { dg-final { scan-assembler-times "srli\ta\[0-9\],a\[0-9\],48" 2 } } */
