/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zbs -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* bclr */
long
foo0 (long i, long j)
{
  return i & ~(1L << j);
}

/* bclri */
long
foo1 (long i)
{
  return i & ~(1L << 20);
}

/* bclr with an XLEN - 1 mask.  */
unsigned long
foo2 (unsigned long i, unsigned long j)
{
  return i & ~(1UL << (j & (__riscv_xlen - 1)));
}

/* bclr with the modulo used by bitmap helpers.  */
void
foo3 (unsigned long *map, unsigned long j)
{
  map[j / __riscv_xlen] &= ~(1UL << (j % __riscv_xlen));
}

/* A narrower mask must be retained.  */
unsigned long
foo4 (unsigned long i, unsigned long j)
{
  return i & ~(1UL << (j & 7));
}

/* { dg-final { scan-assembler-times "bclr\t" 4 } } */
/* { dg-final { scan-assembler-times "bclri\t" 1 } } */
/* { dg-final { scan-assembler-times "andi\t" 1 } } */
/* { dg-final { scan-assembler-not "bset\t" } } */
/* { dg-final { scan-assembler-not "andn\t" } } */
