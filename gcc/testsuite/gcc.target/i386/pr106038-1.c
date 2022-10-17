/* { dg-do compile } */
/* { dg-options "-msse2 -O2" } */
/* { dg-final { scan-assembler-not "xmm" } } */

void
foo3 (char* a, char* __restrict b)
{
  a[0] &= 1;
  a[1] &= 2;
  a[2] &= 3;
  a[3] &= 3;
}

void
foo4 (char* a, char* __restrict b)
{
  a[0] &= 1;
  a[1] &= 2;
}


void
foo1 (short* a, short* __restrict b)
{
  a[0] &= 1;
  a[1] &= 2;
}
