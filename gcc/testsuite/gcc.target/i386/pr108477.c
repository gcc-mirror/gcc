/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int foo (unsigned int a, unsigned int b)
{
  unsigned int r = a & 0x1;
  unsigned int p = b & ~0x3;

  return r + p + 2;
}

unsigned int bar (unsigned int a, unsigned int b)
{
  unsigned int r = a & 0x1;
  unsigned int p = b & ~0x3;

  return r | p | 2;
}

/* { dg-final { scan-assembler-times "leal" 2 } } */
/* { dg-final { scan-assembler-not "orl" } } */
