/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define TYPE long
#define SIZE 257

void __attribute__ ((weak))
f (TYPE *x, TYPE *y, long n, long m)
{
  for (int i = 0; i < SIZE; ++i)
    x[i * n] += y[i * m];
}

/* { dg-final { scan-assembler {\tld1d\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tst1d\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tldr\tx[0-9]+} } } */
/* { dg-final { scan-assembler {\tstr\tx[0-9]+} } } */
/* Should multiply by (257-1)*8 rather than (VF-1)*8.  */
/* { dg-final { scan-assembler-times {lsl\tx[0-9]+, x[0-9]+, 11} 2 } } */
/* { dg-final { scan-assembler {\tcmp\tx[0-9]+, 0} } } */
/* { dg-final { scan-assembler-not {\tcmp\tw[0-9]+, 0} } } */
/* { dg-final { scan-assembler-times {\tcsel\tx[0-9]+} 4 } } */
/* Two range checks and a check for n being zero.  (m being zero is OK.)  */
/* { dg-final { scan-assembler {\tcmp\t} } } */
/* { dg-final { scan-assembler-times {\tccmp\t} 2 } } */
