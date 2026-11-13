/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zbs -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

unsigned long
foo (unsigned long x, unsigned int n)
{
  return (~(x >> (n & (__riscv_xlen - 1)))) & 1UL;
}

/* A narrower mask must be retained.  */
unsigned long
bar (unsigned long x, unsigned int n)
{
  return (~(x >> (n & 7))) & 1UL;
}

/* { dg-final { scan-assembler-times "bext\t" 2 } } */
/* { dg-final { scan-assembler-times "seqz\t|xori\t" 2 } } */
/* { dg-final { scan-assembler-times "andi\t" 1 } } */
