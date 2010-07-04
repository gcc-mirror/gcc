/* { dg-do compile } */
/* { dg-options "-O2 -mtune=atom" } */

extern unsigned char z;

unsigned char
foo (unsigned char x, unsigned char y)
{
  z = x/y;
  return x % y;
}

/* { dg-final { scan-assembler-times "divb" 1 } } */
/* { dg-final { scan-assembler-not "divw" } } */
