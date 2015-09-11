/* { dg-do compile } */
/* { dg-options "-O2 -march=r2 -mcdx" } */

/* Check generation of R2 CDX and.n, andi.n, or.n, xor.n, and not.n
   instructions.  */

extern unsigned int x (unsigned int a);

unsigned int f (unsigned int a, unsigned int b)
{
  return x (a) << b;
}

unsigned int g (unsigned int a)
{
  return x (a) << 24;
}

unsigned int h (unsigned int a, unsigned int b)
{
  return x (a) >> b;
}

unsigned int i (unsigned int a, unsigned int b)
{
  return x (a) >> 24;
}

/* { dg-final { scan-assembler "\tsll\\.n\t.*" } } */
/* { dg-final { scan-assembler "\tslli\\.n\t.*, 24" } } */
/* { dg-final { scan-assembler "\tsrl\\.n\t.*" } } */
/* { dg-final { scan-assembler "\tsrli\\.n\t.*, 24" } } */
