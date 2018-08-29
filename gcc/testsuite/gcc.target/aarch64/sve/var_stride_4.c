/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define TYPE int
#define SIZE 257

void __attribute__ ((weak))
f (TYPE *x, TYPE *y, int n, int m)
{
  for (int i = 0; i < SIZE; ++i)
    x[i * n] += y[i * m];
}

/* { dg-final { scan-assembler {\tld1w\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tst1w\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tldr\tw[0-9]+} } } */
/* { dg-final { scan-assembler {\tstr\tw[0-9]+} } } */
/* Should multiply by (257-1)*4 rather than (VF-1)*4.  */
/* { dg-final { scan-assembler-times {\tsbfiz\tx[0-9]+, x2, 10, 32\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsbfiz\tx[0-9]+, x3, 10, 32\n} 1 } } */
/* { dg-final { scan-assembler {\tcmp\tw2, 0} } } */
/* { dg-final { scan-assembler {\tcmp\tw3, 0} } } */
/* { dg-final { scan-assembler-times {\tcsel\tx[0-9]+} 4 } } */
/* Two range checks and a check for n being zero.  (m being zero is OK.)  */
/* { dg-final { scan-assembler {\tcmp\t} } } */
/* { dg-final { scan-assembler-times {\tccmp\t} 2 } } */
