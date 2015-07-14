/* { dg-do compile } */
/* { dg-options "-O2 -march=r2 -mcdx" } */

/* Check generation of R2 CDX and.n, andi.n, or.n, xor.n, and not.n
   instructions.

   and.n, or.n, and x.n require one of the input registers to be the same
   as the output register.  Since the tests below want to put the result
   in the return value register, they use this function to make sure that
   one of the input operands is also already in the return register.  */

extern unsigned int x (unsigned int a);

unsigned int f (unsigned int a, unsigned int b)
{
  return x (a) & b;
}

unsigned int g (unsigned int a)
{
  return a & 31;
}

unsigned int h (unsigned int a, unsigned int b)
{
  return x (a) | b;
}

unsigned int i (unsigned int a, unsigned int b)
{
  return x (a) ^ b;
}

unsigned int j (unsigned int a)
{
  return ~a;
}

/* { dg-final { scan-assembler "\tand\\.n\t.*" } } */
/* { dg-final { scan-assembler "\tandi\\.n\t.*, 31" } } */
/* { dg-final { scan-assembler "\tor\\.n\t.*" } } */
/* { dg-final { scan-assembler "\txor\\.n\t.*" } } */
/* { dg-final { scan-assembler "\tnot\\.n\t.*" } } */
