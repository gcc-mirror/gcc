/* { dg-options "-O2" } */
/* { dg-do compile } */

unsigned f(unsigned x, unsigned b)
{
  return ((x & 0xff00ff00U) >> 8) | b;
}

unsigned f0(unsigned x, unsigned b)
{
  return ((x & 0xff00ff00U) >> 8) ^ b;
}
unsigned f1(unsigned x, unsigned b)
{
  return ((x & 0xff00ff00U) >> 8) + b;
}

/* { dg-final { scan-assembler-times "lsr\\tw\[0-9\]+" 0 } } */
/* { dg-final { scan-assembler-times "lsr 8" 3 } } */
/* { dg-final { scan-assembler-times "eor\\tw\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "add\\tw\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "orr\\tw\[0-9\]+" 1 } } */
