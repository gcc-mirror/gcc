/* { dg-do compile } */
/* { dg-options "-Os" } */

typedef unsigned char u8;
typedef unsigned short u16;
signed int Xa, Xb;

signed int stzimm_le( int i, int a )
{
  signed int x;
  x = a;
  if (i>0)
    x = 5;
  return x;
}

/* { dg-final { scan-assembler "b.. 1f" } } */
