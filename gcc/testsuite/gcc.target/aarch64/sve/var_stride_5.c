/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define TYPE double
#define SIZE 257

void __attribute__ ((weak))
f (TYPE *x, TYPE *y, long n, long m __attribute__((unused)))
{
  for (int i = 0; i < SIZE; ++i)
    x[i * n] += y[i * n];
}

/* { dg-final { scan-assembler {\tld1d\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tst1d\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tldr\td[0-9]+} } } */
/* { dg-final { scan-assembler {\tstr\td[0-9]+} } } */
/* Should multiply by (VF-1)*8 rather than (257-1)*8.  */
/* { dg-final { scan-assembler-not {, 2048} } } */
/* { dg-final { scan-assembler-not {\t.bfiz\t} } } */
/* { dg-final { scan-assembler-not {lsl[^\n]*[, ]11} } } */
/* { dg-final { scan-assembler {\tcmp\tx[0-9]+, 0} } } */
/* { dg-final { scan-assembler-not {\tcmp\tw[0-9]+, 0} } } */
/* { dg-final { scan-assembler-times {\tcsel\tx[0-9]+} 2 } } */
/* Two range checks and a check for n being zero.  */
/* { dg-final { scan-assembler {\tcmp\t} } } */
/* { dg-final { scan-assembler-times {\tccmp\t} 2 } } */
