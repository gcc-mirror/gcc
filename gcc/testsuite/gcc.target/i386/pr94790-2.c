/* { dg-do compile } */
/* { dg-options "-O2 -mbmi" } */
/* { dg-final { scan-assembler-not "andn\[ \\t\]" } } */
/* { dg-final { scan-assembler-times "xorl\[ \\t\]" 2 } } */

unsigned r1(unsigned a, unsigned b, unsigned mask)
{
  return a ^ ((a ^ b) & mask) + (a ^ b);
}
