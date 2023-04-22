/* { dg-do compile } */
/* { dg-options "-Os" } */

typedef unsigned char u8;
typedef unsigned short u16;
signed int Xa, Xb;

signed int stzreg_bgt(int i, int a, int b)
{
  signed int x;
  x = a;
  if (i<10)
    x = b;
  return x;
}

/* { dg-final { scan-assembler "b.. 1f" } } */

