/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb_zbs -mabi=lp64" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zbb_zbs -mabi=ilp32" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

unsigned long
set_bit (unsigned long x, unsigned short n)
{
  return x | (1UL << (n & (__riscv_xlen - 1)));
}

unsigned long
invert_bit (unsigned long x, unsigned short n)
{
  return x ^ (1UL << (n & (__riscv_xlen - 1)));
}

unsigned long
clear_bit (unsigned long x, unsigned short n)
{
  return x & ~(1UL << (n & (__riscv_xlen - 1)));
}

unsigned long
bit_mask (unsigned short n)
{
  return 1UL << (n & (__riscv_xlen - 1));
}

/* A narrower mask must be retained.  */
unsigned long
clear_bit_mask7 (unsigned long x, unsigned short n)
{
  return x & ~(1UL << (n & 7));
}

#if __riscv_xlen == 64
unsigned long
set_bit_mask31 (unsigned long x, unsigned short n)
{
  return x | (1UL << (n & 31));
}
#endif

/* { dg-final { scan-assembler-times "bset\t" 2 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "bset\t" 3 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "binv\t" 1 } } */
/* { dg-final { scan-assembler-times "bclr\t" 2 } } */
/* { dg-final { scan-assembler-times "andi\t" 1 { target { rv32 } } } } */
/* { dg-final { scan-assembler-times "andi\t" 2 { target { rv64 } } } } */
