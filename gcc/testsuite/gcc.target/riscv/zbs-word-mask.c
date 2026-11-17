/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbb_zbs -mabi=lp64" } */

unsigned long
set (unsigned long x, unsigned int n)
{
  return x | (unsigned long) (1U << (n & 31));
}

unsigned long
invert (unsigned long x, unsigned int n)
{
  return x ^ (unsigned long) (1U << (n & 31));
}

unsigned long
clear (unsigned long x, unsigned int n)
{
  return x & ~(unsigned long) (1U << (n & 31));
}

/* { dg-final { scan-assembler-times "bset\t" 1 } } */
/* { dg-final { scan-assembler-times "binv\t" 1 } } */
/* { dg-final { scan-assembler-times "bclr\t" 1 } } */
/* { dg-final { scan-assembler-times "andi\t" 3 } } */
