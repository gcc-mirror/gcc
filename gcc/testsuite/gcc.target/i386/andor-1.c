/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "andl" } } */

unsigned int foo(unsigned int x)
{
  unsigned int t = x & ~1;
  return t | 1;
}

