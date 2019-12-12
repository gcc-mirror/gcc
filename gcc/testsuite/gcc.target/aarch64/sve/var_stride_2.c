/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define TYPE int
#define SIZE 257

void __attribute__ ((weak))
f (TYPE *x, TYPE *y, unsigned short n, unsigned short m)
{
  for (int i = 0; i < SIZE; ++i)
    x[i * n] += y[i * m];
}

/* { dg-final { scan-assembler {\tld1w\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tst1w\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tldr\tw[0-9]+} } } */
/* { dg-final { scan-assembler {\tstr\tw[0-9]+} } } */
/* Should multiply by (257-1)*4 rather than (VF-1)*4 or (VF-2)*4.  */
/* { dg-final { scan-assembler-times {\tubfiz\tx[0-9]+, x2, 10, 16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tubfiz\tx[0-9]+, x3, 10, 16\n} 1 } } */
/* { dg-final { scan-assembler-not {\tcmp\tx[0-9]+, 0} } } */
/* { dg-final { scan-assembler-not {\tcmp\tw[0-9]+, 0} } } */
/* { dg-final { scan-assembler-not {\tcsel\tx[0-9]+} } } */
/* Two range checks and a check for n being zero.  (m being zero is OK.)  */
/* { dg-final { scan-assembler-times {\tcmp\t} 1 } } */
/* { dg-final { scan-assembler-times {\tccmp\t} 2 } } */
