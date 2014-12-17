/* PR target/61296 */
/* { dg-do compile { target { *-*-linux* } } } */
/* { dg-options "-O2 -malign-data=abi -malign-data=cacheline" } */

struct foo
{
  char i1[8];
  char i2[8];
  char i3[8];
  char i4[8];
  char i5[8];
  char i6[8];
  char i7[8];
  char i8[8];
  char i9[8];
  char i10[8];
  char i11[8];
  char i12[8];
  char i13[8];
  char i14[8];
  char i15[8];
  char i16[8];
};

struct foo x = { 1 };

/* { dg-final { scan-assembler ".align\[ \t]*64\[^:]*\[\n\r]x:" } } */
