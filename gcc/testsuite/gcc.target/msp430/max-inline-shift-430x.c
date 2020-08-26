/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430" } { "" } } */
/* { dg-options "-mcpu=msp430x -mmax-inline-shift=10" } */
/* { dg-final { scan-assembler-not "__mspabi_slli" } } */
/* { dg-final { scan-assembler "__mspabi_sral_6" } } */

volatile int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15;
volatile long l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15;

void
ashift (void)
{
  a1 <<= 1;
  a2 <<= 2;
  a3 <<= 3;
  a4 <<= 4;
  a5 <<= 5;
  a6 <<= 6;
  a7 <<= 7;
  a8 <<= 8;
  a9 <<= 9;
  a10 <<= 10;
  a11 <<= 11;
  a12 <<= 12;
  a13 <<= 13;
  a14 <<= 14;
  a15 <<= 15;
}

void
ashiftrt (void)
{
  l1  >>= 1;
  l2  >>= 2;
  l3  >>= 3;
  l4  >>= 4;
  l5  >>= 5;
  l6  >>= 6;
  l7  >>= 7;
  l8  >>= 8;
  l9  >>= 9;
  l10 >>= 10;
  l11 >>= 11;
  l12 >>= 12;
  l13 >>= 13;
  l14 >>= 14;
  l15 >>= 15;
}
