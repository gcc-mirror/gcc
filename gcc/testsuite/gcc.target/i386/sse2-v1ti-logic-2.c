/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse2" } */

typedef unsigned __int128 v1ti __attribute__ ((__vector_size__ (16)));

v1ti x;
v1ti y;
v1ti z;

void and2()
{
  x &= y;
}

void and3()
{
  x = y & z;
}

void ior2()
{
  x |= y;
}

void ior3()
{
  x = y | z;
}


void xor2()
{
  x ^= y;
}

void xor3()
{
  x =  y ^ z;
}

void not1()
{
  x = ~x;
}

void not2()
{
  x = ~y;
}

/* { dg-final { scan-assembler-times "pand" 2 } } */
/* { dg-final { scan-assembler-times "por" 2 } } */
/* { dg-final { scan-assembler-times "pxor" 4 } } */
