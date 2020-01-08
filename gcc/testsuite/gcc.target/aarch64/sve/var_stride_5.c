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
/* Should use a WAR check that multiplies by (VF-2)*8 rather than
   an overlap check that multiplies by (257-1)*4.  */
/* { dg-final { scan-assembler {\tcntb\t(x[0-9]+)\n.*\tsub\tx[0-9]+, \1, #16\n.*\tmul\tx[0-9]+,[^\n]*\1} } } */
/* { dg-final { scan-assembler-times {\tcsel\tx[0-9]+[^\n]*xzr} 1 } } */
/* One range check and a check for n being zero.  */
/* { dg-final { scan-assembler-times {\tcmp\t} 1 } } */
/* { dg-final { scan-assembler-times {\tccmp\t} 1 } } */
