/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#define TYPE int
#define SIZE 257

void __attribute__ ((weak))
f (TYPE *x, TYPE *y, unsigned short n, long m __attribute__((unused)))
{
  for (int i = 0; i < SIZE; ++i)
    x[i * n] += y[i * n];
}

/* { dg-final { scan-assembler {\tld1w\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tst1w\tz[0-9]+} } } */
/* { dg-final { scan-assembler {\tldr\tw[0-9]+} } } */
/* { dg-final { scan-assembler {\tstr\tw[0-9]+} } } */
/* Should multiply by (VF-1)*4 rather than (257-1)*4.  */
/* { dg-final { scan-assembler-not {, 1024} } } */
/* { dg-final { scan-assembler-not {\t.bfiz\t} } } */
/* { dg-final { scan-assembler-not {lsl[^\n]*[, ]10} } } */
/* { dg-final { scan-assembler-not {\tcmp\tx[0-9]+, 0} } } */
/* { dg-final { scan-assembler-not {\tcmp\tw[0-9]+, 0} } } */
/* { dg-final { scan-assembler-not {\tcsel\tx[0-9]+} } } */
/* Two range checks and a check for n being zero.  */
/* { dg-final { scan-assembler-times {\tcmp\t} 1 } } */
/* { dg-final { scan-assembler-times {\tccmp\t} 2 } } */
