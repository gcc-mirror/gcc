/* { dg-do compile { target rv64 } } */
/* { dg-options "-O2 -march=rv64gc_zbs -mabi=lp64" } */

unsigned long
clear (unsigned long x, unsigned int b, unsigned int e)
{
  e >>= b;
  return x & ~(unsigned long) (1U << (e & 31));
}

/* { dg-final { scan-assembler-times "srlw\t" 1 } } */
/* { dg-final { scan-assembler-times "andi\t" 1 } } */
/* { dg-final { scan-assembler-times "bclr\t" 1 } } */
