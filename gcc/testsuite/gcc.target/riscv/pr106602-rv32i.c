/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-O2 -march=rv32i -mabi=ilp32" } */

unsigned long
foo2 (unsigned long a)
{
  return (unsigned long)(unsigned int) a << 6;
}

/* { dg-final { scan-assembler-times "slli\t" 1 } } */
/* { dg-final { scan-assembler-not "srli\t" } } */
/* { dg-final { scan-assembler-not "\tli\t" } } */
/* { dg-final { scan-assembler-not "addi\t" } } */
/* { dg-final { scan-assembler-not "and\t" } } */
