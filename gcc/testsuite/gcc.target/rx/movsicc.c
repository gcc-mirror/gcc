/* { dg-do compile } */
/* { dg-options "-Os" } */

typedef unsigned char u8;
typedef unsigned short u16;
signed int Xa, Xb;

signed int stzreg_beq(int i, int a, int b)
{
  signed int x;
  x = a;
  if (i)
    x = b;
  return x;
}

/* { dg-final { scan-assembler "bne 1f" } } */

signed int stzreg_bge(int i, int a, int b, int c)
{
  signed int x;
  x = a;
  if (i<c)
    x = b;
  return x;
}

/* { dg-final { scan-assembler "blt 1f" } } */

signed int stzreg_bgt(int i, int a, int b)
{
  signed int x;
  x = a;
  if (i<10)
    x = b;
  return x;
}

/* { dg-final { scan-assembler "ble 1f" } } */

signed int stzreg_ble(int i, int a, int b)
{
  signed int x;
  x = a;
  if (i>0)
    x = b;
  return x;
}

/* { dg-final { scan-assembler "bgt 1f" } } */

signed int stzreg_blt(int i, int a, int b)
{
  signed int x;
  x = a;
  if (i<0)
    x = b;
  return x;
}

/* { dg-final { scan-assembler "blt 1f" } } */

signed int stzreg_bne(int i, int a, int b)
{
  signed int x;
  x = a;
  if (!i)
    x = b;
  return x;
}

/* { dg-final { scan-assembler "beq 1f" } } */

signed int stzimm_le( int i, int a )
{
  signed int x;
  x = a;
  if (i>0)
    x = 5;
  return x;
}

/* { dg-final { scan-assembler "ble 1f" } } */

signed int stzimm_le_r( int i, int a )
{
  signed int x;
  x = a;
  if (i<0)
    x = 5;
  return x;
}

/* { dg-final { scan-assembler "bge 1f" } } */
