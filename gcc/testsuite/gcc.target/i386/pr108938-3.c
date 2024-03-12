/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mno-movbe" } */
/* { dg-final { scan-assembler-times "bswap\[\t ]+" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "bswap\[\t ]+" 3 { target ia32 } } } */

void
foo1 (char* a, unsigned int* __restrict b)
{
  a[0] = b[0] >> 24;
  a[1] = b[0] >> 16;
  a[2] = b[0] >> 8;
  a[3] = b[0];
  a[4] = b[1] >> 24;
  a[5] = b[1] >> 16;
  a[6] = b[1] >> 8;
  a[7] = b[1];
}

void
foo2 (char* a, short* __restrict b)
{
  a[0] = b[0] >> 8;
  a[1] = b[0];
  a[2] = b[1] >> 8;
  a[3] = b[1];
}
