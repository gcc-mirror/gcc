/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

void
foo (double *x, int m)
{
  for (int i = 0; i < 256; ++i)
    x[i * m] += x[i * m];
}

/* { dg-final { scan-assembler-times {\tcbz\tw1,} 1 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, } 1 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, } 1 } } */
/* { dg-final { scan-assembler-times {\tldr\t} 1 } } */
/* { dg-final { scan-assembler-times {\tstr\t} 1 } } */
