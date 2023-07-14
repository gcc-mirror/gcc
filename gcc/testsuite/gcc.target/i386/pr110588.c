/* { dg-do compile } */
/* { dg-options "-O2 -mtune=core2" } */

unsigned char foo (unsigned char x, int y)
{
  int _1 = (int) x;
  int _2 = _1 >> y;
  int _3 = _2 & 1;
  unsigned char _8 = (unsigned char) _3;
  unsigned char _6 = _8 ^ 1;
  return _6;
}

/* { dg-final { scan-assembler "btl" } } */
/* { dg-final { scan-assembler "setnc" } } */
/* { dg-final { scan-assembler-not "sarl" } } */
/* { dg-final { scan-assembler-not "andl" } } */
/* { dg-final { scan-assembler-not "xorl" } } */
