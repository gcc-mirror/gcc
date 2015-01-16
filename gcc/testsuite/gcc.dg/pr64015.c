/* { dg-do compile } */
/* { dg-options "-O2 " } */

int
test (unsigned short a, unsigned char b)
{
  return a > 0xfff2 && b > 252;
}

/* { dg-final { scan-assembler "ccmp" { target aarch64*-*-* } } } */
