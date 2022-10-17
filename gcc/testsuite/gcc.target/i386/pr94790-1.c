/* { dg-do compile } */
/* { dg-options "-O2 -mbmi" } */
/* { dg-final { scan-assembler-times "andn\[ \\t\]" 2 } } */
/* { dg-final { scan-assembler-not "xorl\[ \\t\]" } } */

unsigned r1(unsigned a, unsigned b, unsigned mask)
{
  return a ^ ((a ^ b) & mask);
}

unsigned r2(unsigned a, unsigned b, unsigned mask)
{
  return (~mask & a) | (b & mask);
}
